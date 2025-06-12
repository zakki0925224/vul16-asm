use crate::{
    isa::{self, *},
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
            let token_info = self.lexer.next();
            let token = token_info.token;
            match token {
                Token::Register(reg) => return Ok(reg),
                Token::Comma => continue,
                _ => {
                    return Err(anyhow::anyhow!(
                        "Expected register, found {:?} at line {}",
                        token,
                        token_info.line,
                    ));
                }
            }
        }
    }

    fn parse_immediate(&mut self) -> anyhow::Result<i8> {
        loop {
            let token_info = self.lexer.next();
            let token = token_info.token;
            match token {
                Token::Immediate(imm) => return Ok(imm),
                Token::Comma => continue,
                _ => {
                    return Err(anyhow::anyhow!(
                        "Expected immediate, found {:?} at line {}",
                        token,
                        token_info.line,
                    ));
                }
            }
        }
    }

    fn parse_mnemonic(&mut self) -> anyhow::Result<Option<Instruction>> {
        let token_info = self.lexer.next();
        let token = token_info.token;
        match token {
            Token::Ident(ident) => match &*ident {
                isa::MNEMONIC_ADD => {
                    let opcode = isa::OPCODE_ADD;
                    let rd = self.parse_register()?;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let format_r = FormatR::new(opcode, rd, rs1, rs2).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatR instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Add(format_r)))
                }
                isa::MNEMONIC_ADDI => {
                    let opcode = isa::OPCODE_ADDI;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Addi(format_i)))
                }
                isa::MNEMONIC_SUB => {
                    let opcode = isa::OPCODE_SUB;
                    let rd = self.parse_register()?;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let format_r = FormatR::new(opcode, rd, rs1, rs2).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatR instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Sub(format_r)))
                }
                isa::MNEMONIC_AND => {
                    let opcode = isa::OPCODE_AND;
                    let rd = self.parse_register()?;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let format_r = FormatR::new(opcode, rd, rs1, rs2).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatR instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::And(format_r)))
                }
                isa::MNEMONIC_ANDI => {
                    let opcode = isa::OPCODE_ANDI;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Andi(format_i)))
                }
                isa::MNEMONIC_OR => {
                    let opcode = isa::OPCODE_OR;
                    let rd = self.parse_register()?;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let format_r = FormatR::new(opcode, rd, rs1, rs2).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatR instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Or(format_r)))
                }
                isa::MNEMONIC_ORI => {
                    let opcode = isa::OPCODE_ORI;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Ori(format_i)))
                }
                isa::MNEMONIC_XOR => {
                    let opcode = isa::OPCODE_XOR;
                    let rd = self.parse_register()?;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let format_r = FormatR::new(opcode, rd, rs1, rs2).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatR instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Xor(format_r)))
                }
                isa::MNEMONIC_XORI => {
                    let opcode = isa::OPCODE_XORI;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Xori(format_i)))
                }
                isa::MNEMONIC_SLL => {
                    let opcode = isa::OPCODE_SLL;
                    let rd = self.parse_register()?;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let format_r = FormatR::new(opcode, rd, rs1, rs2).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatR instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Sll(format_r)))
                }
                isa::MNEMONIC_SLLI => {
                    let opcode = isa::OPCODE_SLLI;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Slli(format_i)))
                }
                isa::MNEMONIC_SRL => {
                    let opcode = isa::OPCODE_SRL;
                    let rd = self.parse_register()?;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let format_r = FormatR::new(opcode, rd, rs1, rs2).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatR instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Srl(format_r)))
                }
                isa::MNEMONIC_SRLI => {
                    let opcode = isa::OPCODE_SRLI;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Srli(format_i)))
                }
                isa::MNEMONIC_SRA => {
                    let opcode = isa::OPCODE_SRA;
                    let rd = self.parse_register()?;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let format_r = FormatR::new(opcode, rd, rs1, rs2).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatR instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Sra(format_r)))
                }
                isa::MNEMONIC_SRAI => {
                    let opcode = isa::OPCODE_SRAI;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Srai(format_i)))
                }
                isa::MNEMONIC_SLT => {
                    let opcode = isa::OPCODE_SLT;
                    let rd = self.parse_register()?;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let format_r = FormatR::new(opcode, rd, rs1, rs2).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatR instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Slt(format_r)))
                }
                isa::MNEMONIC_SLTI => {
                    let opcode = isa::OPCODE_SLTI;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Slti(format_i)))
                }
                isa::MNEMONIC_SLTU => {
                    let opcode = isa::OPCODE_SLTU;
                    let rd = self.parse_register()?;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let format_r = FormatR::new(opcode, rd, rs1, rs2).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatR instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Sltu(format_r)))
                }
                isa::MNEMONIC_SLTIU => {
                    let opcode = isa::OPCODE_SLTIU;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Sltiu(format_i)))
                }
                isa::MNEMONIC_LB => {
                    let opcode = isa::OPCODE_LB;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Lb(format_i)))
                }
                isa::MNEMONIC_LBU => {
                    let opcode = isa::OPCODE_LBU;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Lbu(format_i)))
                }
                isa::MNEMONIC_LH => {
                    let opcode = isa::OPCODE_LH;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Lh(format_i)))
                }
                isa::MNEMONIC_SB => {
                    let opcode = isa::OPCODE_SB;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Sb(format_i)))
                }
                isa::MNEMONIC_SH => {
                    let opcode = isa::OPCODE_SH;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Sh(format_i)))
                }
                isa::MNEMONIC_JMP => {
                    let opcode = isa::OPCODE_JMP;
                    let rd = self.parse_register()?;
                    let offset = self.parse_immediate()?;
                    let format_j = FormatJ::new(opcode, rd, offset).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatJ instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Jmp(format_j)))
                }
                isa::MNEMONIC_JMPR => {
                    let opcode = isa::OPCODE_JMPR;
                    let rd = self.parse_register()?;
                    let rs = self.parse_register()?;
                    let imm = self.parse_immediate()?;
                    let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatI instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Jmpr(format_i)))
                }
                isa::MNEMONIC_BEQ => {
                    let opcode = isa::OPCODE_BEQ;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let offset = self.parse_immediate()?;
                    let format_b = FormatB::new(opcode, rs1, rs2, offset).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatB instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Beq(format_b)))
                }
                isa::MNEMONIC_BNE => {
                    let opcode = isa::OPCODE_BNE;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let offset = self.parse_immediate()?;
                    let format_b = FormatB::new(opcode, rs1, rs2, offset).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatB instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Bne(format_b)))
                }
                isa::MNEMONIC_BLT => {
                    let opcode = isa::OPCODE_BLT;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let offset = self.parse_immediate()?;
                    let format_b = FormatB::new(opcode, rs1, rs2, offset).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatB instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Blt(format_b)))
                }
                isa::MNEMONIC_BGE => {
                    let opcode = isa::OPCODE_BGE;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let offset = self.parse_immediate()?;
                    let format_b = FormatB::new(opcode, rs1, rs2, offset).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatB instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Bge(format_b)))
                }
                isa::MNEMONIC_BLTU => {
                    let opcode = isa::OPCODE_BLTU;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let offset = self.parse_immediate()?;
                    let format_b = FormatB::new(opcode, rs1, rs2, offset).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatB instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Bltu(format_b)))
                }
                isa::MNEMONIC_BGEU => {
                    let opcode = isa::OPCODE_BGEU;
                    let rs1 = self.parse_register()?;
                    let rs2 = self.parse_register()?;
                    let offset = self.parse_immediate()?;
                    let format_b = FormatB::new(opcode, rs1, rs2, offset).map_err(|e| {
                        anyhow::anyhow!(
                            "Error parsing FormatB instruction: {:?} at line {}",
                            e,
                            token_info.line,
                        )
                    })?;
                    Ok(Some(Instruction::Bgeu(format_b)))
                }
                _ => Err(anyhow::anyhow!(
                    "Unknown mnemonic: {} at line {}",
                    ident,
                    token_info.line,
                )),
            },
            Token::Comment(_) => self.parse_mnemonic(), // skip
            Token::Eos => Ok(None),                     // end
            _ => Err(anyhow::anyhow!(
                "Expected mnemonic, found {:?}: at line {}",
                token,
                token_info.line,
            )),
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
