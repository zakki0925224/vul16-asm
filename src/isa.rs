use bitfield::bitfield;

bitfield! {
    pub struct FormatR(u16);
    impl Debug;

    u8, reserved, set_reserved: 1, 0;
    u8, rs2, set_rs2: 4, 2;
    u8, rs1, set_rs1: 7, 5;
    u8, rd, set_rd: 10, 8;
    u8, opcode, set_opcode: 15, 11;
}

impl FormatR {
    pub fn new(opcode: u8, rd: u8, rs1: u8, rs2: u8) -> Self {
        let mut format = Self(0);
        format.set_reserved(0);
        format.set_opcode(opcode);
        format.set_rd(rd);
        format.set_rs1(rs1);
        format.set_rs2(rs2);
        format
    }
}

#[derive(Debug)]
pub enum Instruction {
    Add(FormatR),
}

impl Instruction {
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            Instruction::Add(format_r) => format_r.0.to_le_bytes().to_vec(),
        }
    }
}
