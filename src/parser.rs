use crate::{
    isa::{FormatR, Instruction},
    lexer::{Lexer, Token},
};
use std::io::Read;

pub struct Parser<R: Read> {
    lexer: Lexer<R>,
}

impl<R: Read> Parser<R> {
    pub fn load(input: R) -> Self {
        Self {
            lexer: Lexer::new(input),
        }
    }

    fn parse_register(&mut self) -> anyhow::Result<u8> {
        loop {
            let token = self.lexer.next();
            match token {
                Token::Register(reg) => return Ok(reg),
                Token::Comma => continue,
                _ => return Err(anyhow::anyhow!("Expected register, found {:?}", token)),
            }
        }
    }

    fn parse_mnemonic(&mut self) -> anyhow::Result<Instruction> {
        let token = self.lexer.next();
        match token {
            Token::Ident(ident) => match &*ident {
                "add" => {
                    let opcode = 0x0;
                    let rd = self.parse_register()?;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let format_r = FormatR::new(opcode, rd, rs1, rs2);
                    Ok(Instruction::Add(format_r))
                }
                _ => Err(anyhow::anyhow!("Unknown mnemonic: {}", ident)),
            },
            _ => Err(anyhow::anyhow!("Expected mnemonic, found {:?}", token)),
        }
    }

    pub fn parse(&mut self) -> anyhow::Result<Instruction> {
        self.parse_mnemonic()
    }
}
