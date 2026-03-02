use leche_parse::Parsed;
use std::rc::Rc;

#[allow(non_camel_case_types)]
#[derive(Debug, Parsed)]
#[repr(u8)]
pub enum Instruction {
    aaload = 0x32,
    aastore = 0x53,
    aconst_null = 0x01,
    aload(u8) = 0x19,
    aload_0 = 0x2a,
    aload_1,
    aload_2,
    aload_3,
    anewarray(u16) = 0xbd,
    areturn = 0xb0,
    arraylength = 0xbe,
    astore(u8) = 0x3a,
    astore_0 = 0x4b,
    astore_1,
    astore_2,
    astore_3,
    athrow = 0xbf,
    baload = 0x33,
    bastore = 0x54,
    bipush(u8) = 0x10,
    caload = 0x34,
    castore = 0x55,
    checkcast(u8) = 0xc0,
    d2f = 0x90,
    d2i = 0x8e,
    d2l = 0x8f,
    dadd = 0x63,
    daload = 0x31,
    dastore = 0x52,
    dcmpl = 0x97,
    dcmpg,
    dconst_0 = 0x0e,
    dconst_1,
    ddiv = 0x6f,
    dload(u8) = 0x18,
    dload_0 = 0x26,
    dload_1,
    dload_2,
    dload_3,
    dmul = 0x6b,
    dneg = 0x77,
    drem = 0x73,
    dreturn = 0xaf,
    dstore(u8) = 0x39,
    dstore_0 = 0x47,
    dstore_1,
    dstore_2,
    dstore_3,
    dsub = 0x67,
    dup = 0x59,
    dup_x1,
    dup_x2,
    dup2,
    dup2_x1,
    dup2_x2,
    f2d = 0x8d,
    f2i = 0x8b,
    f2l = 0x8c,
    fadd = 0x62,
    faload = 0x30,
    fastore = 0x51,
    fcmpl = 0x95,
    fcmpg,
    fconst_0 = 0x0b,
    fconst_1,
    fconst_2,
    fdiv = 0x6e,
    fload(u8) = 0x17,
    fload_0 = 0x22,
    fload_1,
    fload_2,
    fload_3,
    fmul = 0x6a,
    fneg = 0x76,
    frem = 0x72,
    freturn = 0xae,
    fstore(u8) = 0x38,
    fstore_0 = 0x43,
    fstore_1,
    fstore_2,
    fstore_3,
    fsub = 0x66,
    getfield(u16) = 0xb4,
    getstatic(u16) = 0xb2,
    goto(u16) = 0xa7,
    goto_w(u32) = 0xc8,
    i2b = 0x91,
    i2c,
    i2d = 0x87,
    i2f = 0x86,
    i2l = 0x85,
    i2s = 0x93,
    iadd = 0x60,
    iaload = 0x2e,
    iand = 0x7e,
    iastore = 0x4f,
    iconst_m1 = 0x2,
    iconst_0,
    iconst_1,
    iconst_2,
    iconst_3,
    iconst_4,
    iconst_5,
    idiv = 0x6c,
    if_acmpeq(u16) = 0xa5,
    if_acmpne(u16),
    if_icmpeq(u16) = 0x9f,
    if_icmpne(u16),
    if_icmplt(u16),
    if_icmpge(u16),
    if_icmpgt(u16),
    if_icmple(u16),
    ifeq(u16) = 0x99,
    ifne(u16),
    iflt(u16),
    ifge(u16),
    ifgt(u16),
    ifle(u16),
    ifnull(u16) = 0xc6,
    ifnonnull(u16),
    iinc(u8, i8) = 0x84,
    iload(u8) = 0x15,
    iload_0 = 0x1a,
    iload_1,
    iload_2,
    iload_3,
    imul = 0x68,
    ineg = 0x74,
    instanceof(u16) = 0xc1,
    invokedynamic(#[map(|n| n >> 16)] u32) = 0xba,
    invokeinterface(u16, #[map(|n| n >> 8)] u16) = 0xb9,
    invokespecial(u16) = 0xb7,
    invokestatic(u16),
    invokevirtual(u16) = 0xb6,
    ior = 0x80,
    irem = 0x70,
    ireturn = 0xac,
    ishl = 0x78,
    ishr = 0x7a,
    istore(u8) = 0x36,
    istore_0 = 0x3b,
    istore_1,
    istore_2,
    istore_3,
    isub = 0x64,
    iushr = 0x7c,
    ixor = 0x82,
    jsr(u16) = 0xa8,
    jsr_w(u32) = 0xc9,
    l2d = 0x8a,
    l2f = 0x89,
    l2i = 0x88,
    ladd = 0x61,
    laload = 0x2f,
    land = 0x7f,
    lastore = 0x50,
    lcmp = 0x94,
    lconst_0 = 0x09,
    lconst_1,
    ldc(u8) = 0x12,
    ldc_w(u16),
    ldc2_w(u16) = 0x14,
    ldiv = 0x6d,
    lload(u8) = 0x16,
    lload_0 = 0x1e,
    lload_1,
    lload_2,
    lload_3,
    lmul = 0x69,
    lneg = 0x75,
    lookupswitch(LookupSwitch) = 0xab,
    lor = 0x81,
    lrem = 0x71,
    lreturn = 0xad,
    lshl = 0x79,
    lshr = 0x7b,
    lstore(u8) = 0x37,
    lstore_0 = 0x3f,
    lstore_1,
    lstore_2,
    lstore_3,
    lsub = 0x65,
    lushr = 0x7d,
    lxor = 0x83,
    monitorenter = 0xc2,
    monitorexit,
    multianewarray(u16, u8) = 0xc5,
    new(u16) = 0xbb,
    newarray(Atype) = 0xbc,
    nop = 0,
    pop = 0x57,
    pop2,
    putfield(u16) = 0xb5,
    putstatic(u16) = 0xb3,
    ret(u8) = 0xa9,
    /// return void
    retv = 0xb1,
    saload = 0x35,
    sastore = 0x56,
    sipush(u16) = 0x11,
    swap = 0x5f,
    tableswitch(TableSwitch) = 0xaa,
    wide(Wide) = 0xc4,
}

#[derive(Debug)]
pub struct Wide {
    op: Box<Instruction>,
    index: u16,
    constant: Option<u16>,
}

impl Parsed for Wide {
    fn parse(reader: &mut impl leche_parse::ParseRead) -> std::io::Result<Self> {
        use Instruction::*;
        use std::io::{Error, ErrorKind};

        let op = Box::new(Instruction::parse(reader)?);
        let index = u16::parse(reader)?;
        let constant = match op.as_ref() {
            iload(_) | fload(_) | aload(_) | lload(_) | dload(_) | istore(_) | fstore(_)
            | astore(_) | lstore(_) | dstore(_) | ret(_) => None,
            iinc { .. } => Some(u16::parse(reader)?),
            op => {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("Invalid wide opcode: {op:?}"),
                ));
            }
        };

        Ok(Wide {
            op,
            index,
            constant,
        })
    }
}

#[derive(Debug, Parsed)]
#[repr(u8)]
pub enum Atype {
    Boolean = 4,
    Char,
    Float,
    Double,
    Byte,
    Short,
    Int,
    Long,
}

#[derive(Debug)]
pub struct LookupSwitch {
    pub default: isize,
    pub pairs: std::collections::HashMap<i32, isize>,
}

impl Parsed for LookupSwitch {
    fn parse(reader: &mut impl leche_parse::ParseRead) -> std::io::Result<Self> {
        let Some(offset) = reader.code_offset() else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "not within code array",
            ));
        };

        // TODO: is this correct?
        for _ in 0..(4 - (offset % 4)) {
            u8::parse(reader)?;
        }

        let default = i32::parse(reader)? as isize;

        let pairs = (0..u32::parse(reader)?)
            .map(|_| -> std::io::Result<_> {
                Ok((i32::parse(reader)?, i32::parse(reader)? as isize))
            })
            .collect::<Result<_, _>>()?;

        Ok(LookupSwitch { default, pairs })
    }
}

#[derive(Debug)]
pub struct TableSwitch {
    pub default: isize,
    pub low: i32,
    pub high: i32,
    pub offsets: Rc<[isize]>,
}

impl Parsed for TableSwitch {
    fn parse(reader: &mut impl leche_parse::ParseRead) -> std::io::Result<Self> {
        let Some(offset) = reader.code_offset() else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "not within code array",
            ));
        };

        // TODO: is this correct?
        for _ in 0..(4 - (offset % 4)) {
            u8::parse(reader)?;
        }

        let default = i32::parse(reader)? as isize;
        let low = i32::parse(reader)?;
        let high = i32::parse(reader)?;

        if low > high {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "low must be less than high",
            ));
        }

        let offsets = (low..=high)
            .map(|_| -> std::io::Result<_> { Ok(i32::parse(reader)? as isize) })
            .collect::<Result<_, _>>()?;

        Ok(TableSwitch {
            default,
            low,
            high,
            offsets,
        })
    }
}
