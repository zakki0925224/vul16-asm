use bitfield::bitfield;

bitfield! {
    #[derive(Debug, Clone)]
    pub struct FormatR(u16);

    u8, reserved, set_reserved: 1, 0;
    u8, rs2, set_rs2: 4, 2;
    u8, rs1, set_rs1: 7, 5;
    u8, rd, set_rd: 10, 8;
    u8, opcode, set_opcode: 15, 11;
}

impl FormatR {
    pub fn new(opcode: u8, rd: u8, rs1: u8, rs2: u8) -> anyhow::Result<Self> {
        if rd > 7 {
            return Err(anyhow::anyhow!("rd must be between 0 and 7"));
        }

        if rs1 > 7 {
            return Err(anyhow::anyhow!("rs1 must be between 0 and 7"));
        }

        if rs2 > 7 {
            return Err(anyhow::anyhow!("rs2 must be between 0 and 7"));
        }

        let mut format = Self(0);
        format.set_reserved(0);
        format.set_opcode(opcode);
        format.set_rd(rd);
        format.set_rs1(rs1);
        format.set_rs2(rs2);
        Ok(format)
    }
}

bitfield! {
    #[derive(Debug, Clone)]
    pub struct FormatI(u16);

    u8, imm, set_imm: 4, 0;
    u8, rs, set_rs: 7, 5;
    u8, rd, set_rd: 10, 8;
    u8, opcode, set_opcode: 15, 11;
}

impl FormatI {
    pub fn new(opcode: u8, rd: u8, rs: u8, imm: isize) -> anyhow::Result<Self> {
        if rd > 7 {
            return Err(anyhow::anyhow!("rd must be between 0 and 7"));
        }

        if rs > 7 {
            return Err(anyhow::anyhow!("rs must be between 0 and 7"));
        }

        if imm > 15 || imm < -16 {
            return Err(anyhow::anyhow!("imm must be between -16 and 15"));
        }

        let mut format = Self(0);
        format.set_opcode(opcode);
        format.set_rd(rd);
        format.set_rs(rs);
        format.set_imm(imm as u8);
        Ok(format)
    }
}

bitfield! {
    #[derive(Debug, Clone)]
    pub struct FormatJ(u16);

    u8, offset, set_offset: 7, 0;
    u8, rd, set_rd: 10, 8;
    u8, opcode, set_opcode: 15, 11;
}

impl FormatJ {
    pub fn new(opcode: u8, rd: u8, offset: isize) -> anyhow::Result<Self> {
        if rd > 7 {
            return Err(anyhow::anyhow!("rd must be between 0 and 7"));
        }

        if offset > 127 || offset < -128 {
            return Err(anyhow::anyhow!("offset must be between -128 and 127"));
        }

        let mut format = Self(0);
        format.set_opcode(opcode);
        format.set_rd(rd);
        format.set_offset(offset as u8);
        Ok(format)
    }
}

bitfield! {
    #[derive(Debug, Clone)]
    pub struct FormatB(u16);

    u8, offset, set_offset: 4, 0;
    u8, rs2, set_rs2: 7, 5;
    u8, rs1, set_rs1: 10, 8;
    u8, opcode, set_opcode: 15, 11;
}

impl FormatB {
    pub fn new(opcode: u8, rs1: u8, rs2: u8, offset: isize) -> anyhow::Result<Self> {
        if rs1 > 7 {
            return Err(anyhow::anyhow!("rs1 must be between 0 and 7"));
        }

        if rs2 > 7 {
            return Err(anyhow::anyhow!("rs2 must be between 0 and 7"));
        }

        if offset > 15 || offset < -16 {
            return Err(anyhow::anyhow!("offset must be between -16 and 15"));
        }

        let mut format = Self(0);
        format.set_opcode(opcode);
        format.set_rs1(rs1);
        format.set_rs2(rs2);
        format.set_offset(offset as u8);
        Ok(format)
    }
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Instruction {
    Add(FormatR),
    Addi(FormatI),
    Sub(FormatR),
    And(FormatR),
    Andi(FormatI),
    Or(FormatR),
    Ori(FormatI),
    Xor(FormatR),
    Xori(FormatI),
    Sll(FormatR),
    Slli(FormatI),
    Srl(FormatR),
    Srli(FormatI),
    Sra(FormatR),
    Srai(FormatI),
    Slt(FormatR),
    Slti(FormatI),
    Sltu(FormatR),
    Sltiu(FormatI),
    Lb(FormatI),
    Lbu(FormatI),
    Lw(FormatI),
    Sb(FormatI),
    Sw(FormatI),
    Jmp(FormatJ),
    Jmpr(FormatI),
    Beq(FormatB),
    Bne(FormatB),
    Blt(FormatB),
    Bge(FormatB),
    Bltu(FormatB),
    Bgeu(FormatB),

    // virtual instructions
    J(String), // target label
}

impl Instruction {
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            Instruction::Add(format_r) => format_r.0.to_le_bytes().to_vec(),
            Instruction::Addi(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Sub(format_r) => format_r.0.to_le_bytes().to_vec(),
            Instruction::And(format_r) => format_r.0.to_le_bytes().to_vec(),
            Instruction::Andi(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Or(format_r) => format_r.0.to_le_bytes().to_vec(),
            Instruction::Ori(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Xor(format_r) => format_r.0.to_le_bytes().to_vec(),
            Instruction::Xori(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Sll(format_r) => format_r.0.to_le_bytes().to_vec(),
            Instruction::Slli(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Srl(format_r) => format_r.0.to_le_bytes().to_vec(),
            Instruction::Srli(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Sra(format_r) => format_r.0.to_le_bytes().to_vec(),
            Instruction::Srai(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Slt(format_r) => format_r.0.to_le_bytes().to_vec(),
            Instruction::Slti(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Sltu(format_r) => format_r.0.to_le_bytes().to_vec(),
            Instruction::Sltiu(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Lb(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Lbu(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Lw(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Sb(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Sw(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Jmp(format_j) => format_j.0.to_le_bytes().to_vec(),
            Instruction::Jmpr(format_i) => format_i.0.to_le_bytes().to_vec(),
            Instruction::Beq(format_b) => format_b.0.to_le_bytes().to_vec(),
            Instruction::Bne(format_b) => format_b.0.to_le_bytes().to_vec(),
            Instruction::Blt(format_b) => format_b.0.to_le_bytes().to_vec(),
            Instruction::Bge(format_b) => format_b.0.to_le_bytes().to_vec(),
            Instruction::Bltu(format_b) => format_b.0.to_le_bytes().to_vec(),
            Instruction::Bgeu(format_b) => format_b.0.to_le_bytes().to_vec(),
            Instruction::J(_) => Vec::new(),
        }
    }
}

pub const OPCODE_ADD: u8 = 0x00;
pub const OPCODE_ADDI: u8 = 0x01;
pub const OPCODE_SUB: u8 = 0x02;
pub const OPCODE_AND: u8 = 0x03;
pub const OPCODE_ANDI: u8 = 0x04;
pub const OPCODE_OR: u8 = 0x05;
pub const OPCODE_ORI: u8 = 0x06;
pub const OPCODE_XOR: u8 = 0x07;
pub const OPCODE_XORI: u8 = 0x08;
pub const OPCODE_SLL: u8 = 0x09;
pub const OPCODE_SLLI: u8 = 0x0a;
pub const OPCODE_SRL: u8 = 0x0b;
pub const OPCODE_SRLI: u8 = 0x0c;
pub const OPCODE_SRA: u8 = 0x0d;
pub const OPCODE_SRAI: u8 = 0x0e;
pub const OPCODE_SLT: u8 = 0x0f;
pub const OPCODE_SLTI: u8 = 0x10;
pub const OPCODE_SLTU: u8 = 0x11;
pub const OPCODE_SLTIU: u8 = 0x12;
pub const OPCODE_LB: u8 = 0x13;
pub const OPCODE_LBU: u8 = 0x14;
pub const OPCODE_LW: u8 = 0x15;
pub const OPCODE_SB: u8 = 0x16;
pub const OPCODE_SW: u8 = 0x17;
pub const OPCODE_JMP: u8 = 0x18;
pub const OPCODE_JMPR: u8 = 0x19;
pub const OPCODE_BEQ: u8 = 0x1a;
pub const OPCODE_BNE: u8 = 0x1b;
pub const OPCODE_BLT: u8 = 0x1c;
pub const OPCODE_BGE: u8 = 0x1d;
pub const OPCODE_BLTU: u8 = 0x1e;
pub const OPCODE_BGEU: u8 = 0x1f;

pub const MNEMONIC_ADD: &str = "add";
pub const MNEMONIC_ADDI: &str = "addi";
pub const MNEMONIC_SUB: &str = "sub";
pub const MNEMONIC_AND: &str = "and";
pub const MNEMONIC_ANDI: &str = "andi";
pub const MNEMONIC_OR: &str = "or";
pub const MNEMONIC_ORI: &str = "ori";
pub const MNEMONIC_XOR: &str = "xor";
pub const MNEMONIC_XORI: &str = "xori";
pub const MNEMONIC_SLL: &str = "sll";
pub const MNEMONIC_SLLI: &str = "slli";
pub const MNEMONIC_SRL: &str = "srl";
pub const MNEMONIC_SRLI: &str = "srli";
pub const MNEMONIC_SRA: &str = "sra";
pub const MNEMONIC_SRAI: &str = "srai";
pub const MNEMONIC_SLT: &str = "slt";
pub const MNEMONIC_SLTI: &str = "slti";
pub const MNEMONIC_SLTU: &str = "sltu";
pub const MNEMONIC_SLTIU: &str = "sltiu";
pub const MNEMONIC_LB: &str = "lb";
pub const MNEMONIC_LBU: &str = "lbu";
pub const MNEMONIC_LW: &str = "lw";
pub const MNEMONIC_SB: &str = "sb";
pub const MNEMONIC_SW: &str = "sw";
pub const MNEMONIC_JMP: &str = "jmp";
pub const MNEMONIC_JMPR: &str = "jmpr";
pub const MNEMONIC_BEQ: &str = "beq";
pub const MNEMONIC_BNE: &str = "bne";
pub const MNEMONIC_BLT: &str = "blt";
pub const MNEMONIC_BGE: &str = "bge";
pub const MNEMONIC_BLTU: &str = "bltu";
pub const MNEMONIC_BGEU: &str = "bgeu";

// virtual instructions
pub const MNEMONIC_VIRT_J: &str = "j";

const RESERVED_WORDS: &[&str] = &[
    MNEMONIC_ADD,
    MNEMONIC_ADDI,
    MNEMONIC_SUB,
    MNEMONIC_AND,
    MNEMONIC_ANDI,
    MNEMONIC_OR,
    MNEMONIC_ORI,
    MNEMONIC_XOR,
    MNEMONIC_XORI,
    MNEMONIC_SLL,
    MNEMONIC_SLLI,
    MNEMONIC_SRL,
    MNEMONIC_SRLI,
    MNEMONIC_SRA,
    MNEMONIC_SRAI,
    MNEMONIC_SLT,
    MNEMONIC_SLTI,
    MNEMONIC_SLTU,
    MNEMONIC_SLTIU,
    MNEMONIC_LB,
    MNEMONIC_LBU,
    MNEMONIC_LW,
    MNEMONIC_SB,
    MNEMONIC_SW,
    MNEMONIC_JMP,
    MNEMONIC_JMPR,
    MNEMONIC_BEQ,
    MNEMONIC_BNE,
    MNEMONIC_BLT,
    MNEMONIC_BGE,
    MNEMONIC_BLTU,
    MNEMONIC_BGEU,
    MNEMONIC_VIRT_J,
];

pub fn is_reserved_word(ident: &str) -> bool {
    let is_start_with_num = ident.starts_with(|c: char| c.is_numeric());
    let is_start_with_symbol = ident.starts_with(|c: char| c.is_ascii_punctuation());
    let is_register = ident.len() == 2
        && (ident.starts_with('r') || ident.starts_with('R'))
        && ident.chars().nth(1).unwrap().is_ascii_digit();
    let is_reserved_word = RESERVED_WORDS.contains(&ident);

    is_start_with_num || is_start_with_symbol || is_register || is_reserved_word
}
