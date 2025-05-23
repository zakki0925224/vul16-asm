use parser::Parser;

mod isa;
mod lexer;
mod parser;

fn main() -> anyhow::Result<()> {
    let text = "add r0, r1, r2";
    let mut parser = Parser::load(text.as_bytes());
    let inst = parser.parse()?;
    println!("{:?} -> {:?}", inst, inst.to_bytes());

    Ok(())
}

#[test]
fn test_parser() {
    let text = "add r0, r1, r2";
    let mut parser = Parser::load(text.as_bytes());
    let inst = parser.parse().unwrap();
    assert_eq!(inst.to_bytes(), vec![0x28, 0x00]);
}
