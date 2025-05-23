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
