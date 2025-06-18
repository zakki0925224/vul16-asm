use clap::Parser as _;
use parser::Parser;
use std::{
    fs::File,
    io::{self, Read, Write},
};

mod isa;
mod lexer;
mod parser;

#[derive(clap::Parser)]
struct Args {
    #[arg(short, long)]
    input_file: Option<String>,
    #[arg(short, long)]
    output_file: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let input: Box<dyn Read> = if let Some(input_file) = args.input_file {
        println!("Input from file: {}", input_file);
        Box::new(File::open(input_file)?)
    } else {
        println!("Input from stdin");
        Box::new(io::stdin())
    };

    let mut parser = Parser::load(input);
    let insts = parser.parse()?;
    println!("parsed: {:?}", insts);

    let mut output: Box<dyn Write> = if let Some(output_file) = args.output_file {
        println!("Output to file: {}", output_file);
        Box::new(File::create(output_file)?)
    } else {
        println!("Output to stdout");
        Box::new(io::stdout())
    };

    let bytes = insts
        .iter()
        .flat_map(|inst| inst.to_bytes())
        .collect::<Vec<u8>>();
    output.write_all(&bytes)?;

    Ok(())
}

#[test]
fn test_parser() {
    let text = "add r0, r1, r2";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    assert_eq!(insts[0].to_bytes(), vec![0x28, 0x00]);
}

#[test]
fn test_parser_add() {
    let text = "add r0, r1, r2";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    assert_eq!(insts[0].to_bytes(), vec![0x28, 0x00]);
}

#[test]
fn test_parser_addi() {
    let text = "addi r3, r4, 7";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    // opcode=1, rd=3, rs=4, imm=7 (imm: 0b00111)
    assert_eq!(insts[0].to_bytes(), vec![0x87, 0x0b]);
}

#[test]
fn test_parser_jmp() {
    let text = "jmp r2, -8";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    // opcode=0x18, rd=2, offset=0xf8 (i8 -8 as u8)
    assert_eq!(insts[0].to_bytes(), vec![0xf8, 0xc2]);
}

#[test]
fn test_parser_beq() {
    let text = "beq r1, r2, 5";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    // opcode=0x1a, rs1=1, rs2=2, offset=5 (offset: 0b00101)
    assert_eq!(insts[0].to_bytes(), vec![0x45, 0xd1]);
}

#[test]
fn test_parser_multiple() {
    let text = "add r0, r1, r2\naddi r3, r4, 1";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    assert_eq!(insts.len(), 2);
    assert_eq!(insts[0].to_bytes(), vec![0x28, 0x00]);
    assert_eq!(insts[1].to_bytes(), vec![0x81, 0x0b]);
}

#[test]
fn test_comment() {
    let text = "add r0, r1, r2 ; this is a comment\naddi r3, r4, 1";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    assert_eq!(insts.len(), 2);
    assert_eq!(insts[0].to_bytes(), vec![0x28, 0x00]);
    assert_eq!(insts[1].to_bytes(), vec![0x81, 0x0b]);
}

#[test]
fn test_immediate() {
    // decimal
    let text = "addi r0, r0, 15";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    assert_eq!(insts[0].to_bytes(), vec![0xf, 0x08]);

    let text = "addi r0, r0, -7";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    assert_eq!(insts[0].to_bytes(), vec![0x19, 0x08]);

    // hex
    let text = "addi r0, r0, 0xa";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    assert_eq!(insts[0].to_bytes(), vec![0xa, 0x08]);

    let text = "addi r0, r0, -0xa";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    assert_eq!(insts[0].to_bytes(), vec![0x16, 0x08]);

    // octal
    let text = "addi r0, r0, 0o12";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    assert_eq!(insts[0].to_bytes(), vec![0xa, 0x08]);

    // binary
    let text = "addi r0, r0, 0b1010";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    assert_eq!(insts[0].to_bytes(), vec![0xa, 0x08]);

    let text = "addi r0, r0, -0b1010";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    assert_eq!(insts[0].to_bytes(), vec![0x16, 0x08]);
}

#[test]
fn test_expanded_instruction() {
    let text = "addi r0, r0, 0xf000";
    let mut parser = Parser::load(text.as_bytes());
    let insts = parser.parse().unwrap();
    assert_eq!(insts.len(), 4096);
}
