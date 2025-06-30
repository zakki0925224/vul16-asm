use crate::{
    isa::{self, *},
    lexer::{Lexer, Token, TokenInfo},
};
use core::slice::Iter;
use std::{collections::HashMap, io::Read};

const LABEL_ROOT: &str = "<ROOT>";

pub struct Parser<R: Read> {
    lexer: Lexer<R>,
    token_map: HashMap<String, Vec<TokenInfo>>,
}

impl<R: Read> Parser<R> {
    pub fn load(input: R) -> Self {
        Self {
            lexer: Lexer::new(input),
            token_map: HashMap::new(),
        }
    }

    fn parse_register<'a>(&self, token_infos_iter: &mut Iter<'a, TokenInfo>) -> anyhow::Result<u8> {
        while let Some(info) = token_infos_iter.next() {
            match &info.token {
                Token::Register(reg) => return Ok(*reg),
                Token::Comma => continue,
                _ => {
                    return Err(anyhow::anyhow!(
                        "Expected register, but found {:?} at line {}",
                        info.token,
                        info.line
                    ));
                }
            }
        }

        Err(anyhow::anyhow!("Expected register, but found EOS"))
    }

    fn parse_immediate<'a>(
        &self,
        token_infos_iter: &mut Iter<'a, TokenInfo>,
    ) -> anyhow::Result<isize> {
        while let Some(info) = token_infos_iter.next() {
            match &info.token {
                Token::Immediate(imm) => return Ok(*imm),
                Token::Comma => continue,
                _ => {
                    return Err(anyhow::anyhow!(
                        "Expected immediate, but found {:?} at line {}",
                        info.token,
                        info.line
                    ));
                }
            }
        }

        Err(anyhow::anyhow!("Expected immediate, but found EOS"))
    }

    fn parse_target_label<'a>(
        &self,
        token_infos_iter: &mut Iter<'a, TokenInfo>,
    ) -> anyhow::Result<String> {
        while let Some(info) = token_infos_iter.next() {
            match &info.token {
                Token::TargetLabel(label) => return Ok(label.clone()),
                _ => {
                    return Err(anyhow::anyhow!(
                        "Expected target label, but found {:?} at line {}",
                        info.token,
                        info.line
                    ));
                }
            }
        }

        Err(anyhow::anyhow!("Expected target label, but found EOS"))
    }

    fn format_r_inst<'a>(
        &self,
        token_infos_iter: &mut Iter<'a, TokenInfo>,
        opcode: u8,
        info_line: usize,
    ) -> anyhow::Result<Instruction> {
        let rd = self.parse_register(token_infos_iter)?;
        let rs1 = self.parse_register(token_infos_iter)?;
        let rs2 = self.parse_register(token_infos_iter)?;
        let format_r = FormatR::new(opcode, rd, rs1, rs2).map_err(|err| {
            anyhow::anyhow!(
                "Error parsing FormatR instruction {:?} at line {}",
                err,
                info_line
            )
        })?;

        match opcode {
            isa::OPCODE_ADD => Ok(Instruction::Add(format_r)),
            isa::OPCODE_SUB => Ok(Instruction::Sub(format_r)),
            isa::OPCODE_AND => Ok(Instruction::And(format_r)),
            isa::OPCODE_OR => Ok(Instruction::Or(format_r)),
            isa::OPCODE_XOR => Ok(Instruction::Xor(format_r)),
            isa::OPCODE_SLL => Ok(Instruction::Sll(format_r)),
            isa::OPCODE_SRL => Ok(Instruction::Srl(format_r)),
            isa::OPCODE_SRA => Ok(Instruction::Sra(format_r)),
            isa::OPCODE_SLT => Ok(Instruction::Slt(format_r)),
            isa::OPCODE_SLTU => Ok(Instruction::Sltu(format_r)),
            _ => Err(anyhow::anyhow!(
                "Expected FormatR instruction's opcode, but found 0x{:x} at line {}",
                opcode,
                info_line
            )),
        }
    }

    fn format_i_inst<'a>(
        &self,
        token_infos_iter: &mut Iter<'a, TokenInfo>,
        opcode: u8,
        info_line: usize,
    ) -> anyhow::Result<Instruction> {
        let rd = self.parse_register(token_infos_iter)?;
        let rs = self.parse_register(token_infos_iter)?;
        let imm = self.parse_immediate(token_infos_iter)?;
        let format_i = FormatI::new(opcode, rd, rs, imm).map_err(|err| {
            anyhow::anyhow!(
                "Error parsing FormatI instruction {:?} at line {}",
                err,
                info_line
            )
        })?;

        match opcode {
            isa::OPCODE_ADDI => Ok(Instruction::Addi(format_i)),
            isa::OPCODE_ANDI => Ok(Instruction::Andi(format_i)),
            isa::OPCODE_ORI => Ok(Instruction::Ori(format_i)),
            isa::OPCODE_XORI => Ok(Instruction::Xori(format_i)),
            isa::OPCODE_SLLI => Ok(Instruction::Slli(format_i)),
            isa::OPCODE_SRLI => Ok(Instruction::Srli(format_i)),
            isa::OPCODE_SRAI => Ok(Instruction::Srai(format_i)),
            isa::OPCODE_SLTI => Ok(Instruction::Slti(format_i)),
            isa::OPCODE_SLTIU => Ok(Instruction::Sltiu(format_i)),
            isa::OPCODE_LB => Ok(Instruction::Lb(format_i)),
            isa::OPCODE_LBU => Ok(Instruction::Lbu(format_i)),
            isa::OPCODE_LH => Ok(Instruction::Lh(format_i)),
            isa::OPCODE_SB => Ok(Instruction::Sb(format_i)),
            isa::OPCODE_SH => Ok(Instruction::Sh(format_i)),
            isa::OPCODE_JMPR => Ok(Instruction::Jmpr(format_i)),
            _ => Err(anyhow::anyhow!(
                "Expected FormatI instruction's opcode, but found 0x{:x} at line {}",
                opcode,
                info_line
            )),
        }
    }

    fn format_j_inst<'a>(
        &self,
        token_infos_iter: &mut Iter<'a, TokenInfo>,
        opcode: u8,
        info_line: usize,
    ) -> anyhow::Result<Instruction> {
        let rd = self.parse_register(token_infos_iter)?;
        let offset = self.parse_immediate(token_infos_iter)?;
        let format_j = FormatJ::new(opcode, rd, offset).map_err(|err| {
            anyhow::anyhow!(
                "Error parsing FormatJ instruction {:?} at line {}",
                err,
                info_line
            )
        })?;

        match opcode {
            isa::OPCODE_JMP => Ok(Instruction::Jmp(format_j)),
            _ => Err(anyhow::anyhow!(
                "Expected FormatJ instruction's opcode, but found 0x{:x} at line {}",
                opcode,
                info_line
            )),
        }
    }

    fn format_b_inst<'a>(
        &self,
        token_infos_iter: &mut Iter<'a, TokenInfo>,
        opcode: u8,
        info_line: usize,
    ) -> anyhow::Result<Instruction> {
        let rs1 = self.parse_register(token_infos_iter)?;
        let rs2 = self.parse_register(token_infos_iter)?;
        let offset = self.parse_immediate(token_infos_iter)?;
        let format_b = FormatB::new(opcode, rs1, rs2, offset).map_err(|err| {
            anyhow::anyhow!(
                "Error parsing FormatB instruction {:?} at line {}",
                err,
                info_line
            )
        })?;

        match opcode {
            isa::OPCODE_BEQ => Ok(Instruction::Beq(format_b)),
            isa::OPCODE_BNE => Ok(Instruction::Bne(format_b)),
            isa::OPCODE_BLT => Ok(Instruction::Blt(format_b)),
            isa::OPCODE_BGE => Ok(Instruction::Bge(format_b)),
            isa::OPCODE_BLTU => Ok(Instruction::Bltu(format_b)),
            isa::OPCODE_BGEU => Ok(Instruction::Bgeu(format_b)),
            _ => Err(anyhow::anyhow!(
                "Expected FormatB instruction's opcode, but found 0x{:x} at line {}",
                opcode,
                info_line
            )),
        }
    }

    fn parse_mnemonic<'a>(
        &self,
        token_infos_iter: &mut Iter<'a, TokenInfo>,
    ) -> anyhow::Result<Option<Instruction>> {
        while let Some(info) = token_infos_iter.next() {
            match &info.token {
                Token::Ident(ident) => match ident.as_str() {
                    isa::MNEMONIC_ADD => {
                        let opcode = isa::OPCODE_ADD;
                        return Ok(Some(self.format_r_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SUB => {
                        let opcode = isa::OPCODE_SUB;
                        return Ok(Some(self.format_r_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_AND => {
                        let opcode = isa::OPCODE_AND;
                        return Ok(Some(self.format_r_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_OR => {
                        let opcode = isa::OPCODE_OR;
                        return Ok(Some(self.format_r_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_XOR => {
                        let opcode = isa::OPCODE_XOR;
                        return Ok(Some(self.format_r_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SLL => {
                        let opcode = isa::OPCODE_SLL;
                        return Ok(Some(self.format_r_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SRL => {
                        let opcode = isa::OPCODE_SRL;
                        return Ok(Some(self.format_r_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SRA => {
                        let opcode = isa::OPCODE_SRA;
                        return Ok(Some(self.format_r_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SLT => {
                        let opcode = isa::OPCODE_SLT;
                        return Ok(Some(self.format_r_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SLTU => {
                        let opcode = isa::OPCODE_SLTU;
                        return Ok(Some(self.format_r_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_ADDI => {
                        let opcode = isa::OPCODE_ADDI;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_ANDI => {
                        let opcode = isa::OPCODE_ANDI;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_ORI => {
                        let opcode = isa::OPCODE_ORI;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_XORI => {
                        let opcode = isa::OPCODE_XORI;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SLLI => {
                        let opcode = isa::OPCODE_SLLI;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SRLI => {
                        let opcode = isa::OPCODE_SRLI;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SRAI => {
                        let opcode = isa::OPCODE_SRAI;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SLTI => {
                        let opcode = isa::OPCODE_SLTI;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SLTIU => {
                        let opcode = isa::OPCODE_SLTIU;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_LB => {
                        let opcode = isa::OPCODE_LB;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_LBU => {
                        let opcode = isa::OPCODE_LBU;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_LH => {
                        let opcode = isa::OPCODE_LH;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SB => {
                        let opcode = isa::OPCODE_SB;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_SH => {
                        let opcode = isa::OPCODE_SH;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_JMPR => {
                        let opcode = isa::OPCODE_JMPR;
                        return Ok(Some(self.format_i_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_JMP => {
                        let opcode = isa::OPCODE_JMP;
                        return Ok(Some(self.format_j_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_BEQ => {
                        let opcode = isa::OPCODE_BEQ;
                        return Ok(Some(self.format_b_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_BNE => {
                        let opcode = isa::OPCODE_BNE;
                        return Ok(Some(self.format_b_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_BLT => {
                        let opcode = isa::OPCODE_BLT;
                        return Ok(Some(self.format_b_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_BGE => {
                        let opcode = isa::OPCODE_BGE;
                        return Ok(Some(self.format_b_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_BLTU => {
                        let opcode = isa::OPCODE_BLTU;
                        return Ok(Some(self.format_b_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_BGEU => {
                        let opcode = isa::OPCODE_BGEU;
                        return Ok(Some(self.format_b_inst(
                            token_infos_iter,
                            opcode,
                            info.line,
                        )?));
                    }
                    isa::MNEMONIC_VIRT_J => {
                        let target_label = self.parse_target_label(token_infos_iter)?;
                        return Ok(Some(Instruction::J(target_label)));
                    }
                    _ => {
                        return Err(anyhow::anyhow!(
                            "Unknown mnemonic: {} at line {}",
                            ident,
                            info.line,
                        ));
                    }
                },
                Token::Comment(_) => return self.parse_mnemonic(token_infos_iter), // skip
                Token::Eos => unreachable!(),
                _ => {
                    return Err(anyhow::anyhow!(
                        "Expected mnemonic, but found {:?}: at line {}",
                        info.token,
                        info.line,
                    ));
                }
            }
        }

        Ok(None)
    }

    fn parse_tokens<'a>(
        &self,
        token_infos_iter: &mut Iter<'a, TokenInfo>,
    ) -> anyhow::Result<Vec<Instruction>> {
        let mut insts = Vec::new();

        loop {
            let inst = match self.parse_mnemonic(token_infos_iter)? {
                Some(inst) => inst,
                None => break,
            };
            insts.push(inst);
        }

        Ok(insts)
    }

    pub fn parse(&mut self) -> anyhow::Result<Vec<Instruction>> {
        let mut token_infos = Vec::new();
        loop {
            let token_info = self.lexer.next();
            if token_info.token == Token::Eos {
                break;
            }

            token_infos.push(token_info);
        }

        // create map
        let mut key_label = None;
        for info in token_infos {
            match info.token {
                Token::Label(label) => key_label = Some(label),
                _ => {
                    if key_label.is_none() {
                        key_label = Some(LABEL_ROOT.to_string());
                    }

                    let infos = self
                        .token_map
                        .entry(key_label.clone().unwrap())
                        .or_insert_with(Vec::new);
                    infos.push(info);
                }
            }
        }

        let mut virt_insts = Vec::new();
        let mut label_pos = HashMap::new();
        let mut inst_count = 0;

        println!("Virtual instructions:");
        let token_infos = self.token_map.get(LABEL_ROOT).ok_or(anyhow::anyhow!(
            "At least one unlabeled instruction is required"
        ))?;
        let insts = self.parse_tokens(&mut token_infos.iter())?;
        label_pos.insert(LABEL_ROOT.to_string(), (inst_count, insts.len()));
        println!("\t{}({}):\n\t\t{:?}", LABEL_ROOT, inst_count, insts);
        inst_count += insts.len();
        virt_insts.extend(insts);

        for (label, token_infos) in &self.token_map {
            if label == LABEL_ROOT {
                continue;
            }

            let insts = self.parse_tokens(&mut token_infos.iter())?;
            label_pos.insert(label.clone(), (inst_count, insts.len()));
            println!("\t{}({}):\n\t\t{:?}", label, inst_count, insts);
            inst_count += insts.len();
            virt_insts.extend(insts);
        }

        let mut real_insts = Vec::new();
        for (i, inst) in virt_insts.iter().enumerate() {
            match inst {
                Instruction::J(target_label) => {
                    let &(target_pos, _) = label_pos
                        .get(target_label)
                        .ok_or(anyhow::anyhow!("Unknown label: {}", target_label))?;
                    let offset = (target_pos as isize - i as isize) * 2;
                    // println!("offset: {}", offset);

                    let format_j = FormatJ::new(isa::OPCODE_JMP, 0, offset).map_err(|_| {
                        anyhow::anyhow!(
                            "Offset out of range for label {}: {}",
                            target_label,
                            offset
                        )
                    })?;
                    let inst = Instruction::Jmp(format_j);

                    real_insts.push(inst);
                }
                inst => {
                    real_insts.push(inst.clone());
                }
            }
        }

        Ok(real_insts)
    }
}
