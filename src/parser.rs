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
                Token::Register(reg) => {
                    if reg > 7 {
                        return Err(anyhow::anyhow!(
                            "Invalid register, must be 0~7 but got {}",
                            reg
                        ));
                    }

                    return Ok(reg);
                }
                Token::Comma => continue,
                _ => return Err(anyhow::anyhow!("Expected register, found {:?}", token)),
            }
        }
    }

    fn parse_mnemonic(&mut self) -> anyhow::Result<Option<Instruction>> {
        let token = self.lexer.next();
        match token {
            Token::Ident(ident) => match &*ident {
                "add" => {
                    let opcode = 0x0;
                    let rd = self.parse_register()?;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let format_r = FormatR::new(opcode, rd, rs1, rs2);
                    Ok(Some(Instruction::Add(format_r)))
                }
                _ => Err(anyhow::anyhow!("Unknown mnemonic: {}", ident)),
            },
            Token::Eos => Ok(None),
            _ => Err(anyhow::anyhow!("Expected mnemonic, found {:?}", token)),
        }
    }

    pub fn parse(&mut self) -> anyhow::Result<Vec<Instruction>> {
        let mut insts = Vec::new();
        while let Some(inst) = self.parse_mnemonic()? {
            insts.push(inst);
        }
        Ok(insts)
    }
}
