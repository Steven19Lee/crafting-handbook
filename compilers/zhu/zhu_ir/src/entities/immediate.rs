use crate::entities::{instruction::opcode::OpCode, r#type::ValueType};
use std::fmt;

/// Data entity to represent immediate (compiler-time-known number) in IR instruction,
///
/// During Lowering, This data entity will directly encoding in instruction.
///
/// This data entity following informations:
/// - Value Type
/// - Numeric value
/// - static function to computed by opcode.
///
/// ### What is different between `ConstantData` and `Immediate` ?
/// Constant data in store in bite-level, and `ConstantData` usually contain information
/// about Data type in Zhu IR. in the other hand, Immdiate contain Data type for Zhu IR,
/// and the value that can direct compute according to OpCode.
#[derive(Debug, PartialEq, Clone)]
pub enum Immediate {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}
/// Explicit data entity only used for memory operation.
#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Offset(pub i32);

/// Hashable and Euqalable version of `Immediate` data entity,
/// ### Why Need this Data entity ?
/// Floating number can't implement totally eq (since NaN), but we will need hash `Immediate`
/// for some reason (mostly is because we need to hash `InstructionData`), so we will need a
/// data entitry can store same information as `Immediate`.
///
/// ### Caveat
///  Immeidate Key need to transform into Immediate to compute by opcode.
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum ImmediateKey {
    Int { ty: ValueType, byte: [u8; 8] },
    F32 { byte: [u8; 8] },
    F64 { byte: [u8; 8] },
}
impl Immediate {
    pub fn get_value_type(&self) -> ValueType {
        match *self {
            Immediate::U8(_) => ValueType::U8,
            Immediate::U16(_) => ValueType::U16,
            Immediate::U32(_) => ValueType::U32,
            Immediate::U64(_) => ValueType::U64,
            Immediate::I16(_) => ValueType::I16,
            Immediate::I32(_) => ValueType::I32,
            Immediate::I64(_) => ValueType::I64,
            Immediate::F32(_) => ValueType::F32,
            Immediate::F64(_) => ValueType::F64,
        }
    }
    pub fn get_bytes(&self) -> [u8; 8] {
        match *self {
            Immediate::U8(value) => (value as u64).to_le_bytes(),
            Immediate::U16(value) => (value as u64).to_le_bytes(),
            Immediate::U32(value) => (value as u64).to_le_bytes(),
            Immediate::U64(value) => value.to_le_bytes(),
            Immediate::I16(value) => (value as i64).to_le_bytes(),
            Immediate::I32(value) => (value as i64).to_le_bytes(),
            Immediate::I64(value) => value.to_le_bytes(),
            Immediate::F32(value) => (value as f64).to_le_bytes(),
            Immediate::F64(value) => value.to_le_bytes(),
        }
    }
}

impl Immediate {
    pub fn compute_unary_immi(opcode: OpCode, immi: Immediate) -> Immediate {
        match opcode {
            OpCode::Neg => match immi {
                Immediate::I16(value) => Immediate::I16(-value),
                Immediate::I32(value) => Immediate::I32(-value),
                Immediate::I64(value) => Immediate::I64(-value),
                Immediate::F32(value) => Immediate::F32(-value),
                Immediate::F64(value) => Immediate::F64(-value),
                _ => panic!("Neg operation is only supported for signed integers and floating point numbers."),
            },
            OpCode::ToU8 => todo!(),
            OpCode::ToU16 => todo!(),
            OpCode::ToU32 => todo!(),
            OpCode::ToU64 => todo!(),
            OpCode::ToI16 => todo!(),
            OpCode::ToI32 => todo!(),
            OpCode::ToI64 => todo!(),
            OpCode::ToF32 => todo!(),
            OpCode::ToF64 => todo!(),
            _ => {
                panic!("Unsupported opcode for unary operation: {:?}", opcode);
            }
        }
    }

    pub fn compute_binary_immi(opcode: OpCode, left: Immediate, right: Immediate) -> Immediate {
        match opcode {
            OpCode::Add | OpCode::Addi | OpCode::FAdd => match (left, right) {
                (Immediate::U8(l), Immediate::U8(r)) => Immediate::U8(l.wrapping_add(r)),
                (Immediate::U16(l), Immediate::U16(r)) => Immediate::U16(l.wrapping_add(r)),
                (Immediate::U32(l), Immediate::U32(r)) => Immediate::U32(l.wrapping_add(r)),
                (Immediate::U64(l), Immediate::U64(r)) => Immediate::U64(l.wrapping_add(r)),
                (Immediate::I16(l), Immediate::I16(r)) => Immediate::I16(l.wrapping_add(r)),
                (Immediate::I32(l), Immediate::I32(r)) => Immediate::I32(l.wrapping_add(r)),
                (Immediate::I64(l), Immediate::I64(r)) => Immediate::I64(l.wrapping_add(r)),
                (Immediate::F32(l), Immediate::F32(r)) => Immediate::F32(l + r),
                (Immediate::F64(l), Immediate::F64(r)) => Immediate::F64(l + r),
                _ => panic!("Immediate type is missmatch."),
            },
            OpCode::Sub | OpCode::Subi | OpCode::FSub => match (left, right) {
                (Immediate::U8(l), Immediate::U8(r)) => Immediate::U8(l.wrapping_sub(r)),
                (Immediate::U16(l), Immediate::U16(r)) => Immediate::U16(l.wrapping_sub(r)),
                (Immediate::U32(l), Immediate::U32(r)) => Immediate::U32(l.wrapping_sub(r)),
                (Immediate::U64(l), Immediate::U64(r)) => Immediate::U64(l.wrapping_sub(r)),
                (Immediate::I16(l), Immediate::I16(r)) => Immediate::I16(l.wrapping_sub(r)),
                (Immediate::I32(l), Immediate::I32(r)) => Immediate::I32(l.wrapping_sub(r)),
                (Immediate::I64(l), Immediate::I64(r)) => Immediate::I64(l.wrapping_sub(r)),
                (Immediate::F32(l), Immediate::F32(r)) => Immediate::F32(l - r),
                (Immediate::F64(l), Immediate::F64(r)) => Immediate::F64(l - r),
                _ => panic!("Immediate type is missmatch."),
            },
            OpCode::Mul | OpCode::Muli | OpCode::FMul => match (left, right) {
                (Immediate::U8(l), Immediate::U8(r)) => Immediate::U8(l.wrapping_mul(r)),
                (Immediate::U16(l), Immediate::U16(r)) => Immediate::U16(l.wrapping_mul(r)),
                (Immediate::U32(l), Immediate::U32(r)) => Immediate::U32(l.wrapping_mul(r)),
                (Immediate::U64(l), Immediate::U64(r)) => Immediate::U64(l.wrapping_mul(r)),
                (Immediate::I16(l), Immediate::I16(r)) => Immediate::I16(l.wrapping_mul(r)),
                (Immediate::I32(l), Immediate::I32(r)) => Immediate::I32(l.wrapping_mul(r)),
                (Immediate::I64(l), Immediate::I64(r)) => Immediate::I64(l.wrapping_mul(r)),
                (Immediate::F32(l), Immediate::F32(r)) => Immediate::F32(l * r),
                (Immediate::F64(l), Immediate::F64(r)) => Immediate::F64(l * r),
                _ => panic!("Immediate type is missmatch."),
            },
            OpCode::Divide | OpCode::Dividei | OpCode::FDivide => match (left, right) {
                (Immediate::U8(l), Immediate::U8(r)) => Immediate::U8(l.wrapping_div(r)),
                (Immediate::U16(l), Immediate::U16(r)) => Immediate::U16(l.wrapping_div(r)),
                (Immediate::U32(l), Immediate::U32(r)) => Immediate::U32(l.wrapping_div(r)),
                (Immediate::U64(l), Immediate::U64(r)) => Immediate::U64(l.wrapping_div(r)),
                (Immediate::I16(l), Immediate::I16(r)) => Immediate::I16(l.wrapping_div(r)),
                (Immediate::I32(l), Immediate::I32(r)) => Immediate::I32(l.wrapping_div(r)),
                (Immediate::I64(l), Immediate::I64(r)) => Immediate::I64(l.wrapping_div(r)),
                (Immediate::F32(l), Immediate::F32(r)) => Immediate::F32(l / r),
                (Immediate::F64(l), Immediate::F64(r)) => Immediate::F64(l / r),
                _ => panic!("Immediate type is missmatch."),
            },
            OpCode::Reminder | OpCode::Reminderi | OpCode::FReminder => match (left, right) {
                (Immediate::U8(l), Immediate::U8(r)) => Immediate::U8(l.wrapping_rem(r)),
                (Immediate::U16(l), Immediate::U16(r)) => Immediate::U16(l.wrapping_rem(r)),
                (Immediate::U32(l), Immediate::U32(r)) => Immediate::U32(l.wrapping_rem(r)),
                (Immediate::U64(l), Immediate::U64(r)) => Immediate::U64(l.wrapping_rem(r)),
                (Immediate::I16(l), Immediate::I16(r)) => Immediate::I16(l.wrapping_rem(r)),
                (Immediate::I32(l), Immediate::I32(r)) => Immediate::I32(l.wrapping_rem(r)),
                (Immediate::I64(l), Immediate::I64(r)) => Immediate::I64(l.wrapping_rem(r)),
                _ => panic!("Immediate type is missmatch."),
            },
            OpCode::BitwiseNot => todo!(),
            OpCode::BitwiseOR => todo!(),
            OpCode::BitwiseAnd => todo!(),
            OpCode::ShiftLeft => todo!(),
            OpCode::ShiftRight => todo!(),
            _ => panic!("Unsupported opcode for binary operation: {:?}", opcode),
        }
    }
}

impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Immediate::U8(num) => write!(f, "{}", num),
            Immediate::U16(num) => write!(f, "{}", num),
            Immediate::U32(num) => write!(f, "{}", num),
            Immediate::U64(num) => write!(f, "{}", num),
            Immediate::I16(num) => write!(f, "{}", num),
            Immediate::I32(num) => write!(f, "{}", num),
            Immediate::I64(num) => write!(f, "{}", num),
            Immediate::F32(num) => write!(f, "{}", num),
            Immediate::F64(num) => write!(f, "{}", num),
        }?;
        Ok(())
    }
}

impl AsRef<Immediate> for Immediate {
    fn as_ref(&self) -> &Immediate {
        self
    }
}

impl<T> From<T> for ImmediateKey
where
    T: AsRef<Immediate>,
{
    fn from(value: T) -> Self {
        let immediate = value.as_ref();
        match immediate {
            Immediate::U8(_) => ImmediateKey::Int {
                ty: ValueType::U8,
                byte: immediate.get_bytes(),
            },
            Immediate::U16(_) => ImmediateKey::Int {
                ty: ValueType::U16,
                byte: immediate.get_bytes(),
            },
            Immediate::U32(_) => ImmediateKey::Int {
                ty: ValueType::U32,
                byte: immediate.get_bytes(),
            },
            Immediate::U64(_) => ImmediateKey::Int {
                ty: ValueType::U64,
                byte: immediate.get_bytes(),
            },
            Immediate::I16(_) => ImmediateKey::Int {
                ty: ValueType::I16,
                byte: immediate.get_bytes(),
            },
            Immediate::I32(_) => ImmediateKey::Int {
                ty: ValueType::I32,
                byte: immediate.get_bytes(),
            },
            Immediate::I64(_) => ImmediateKey::Int {
                ty: ValueType::I64,
                byte: immediate.get_bytes(),
            },
            Immediate::F32(_) => ImmediateKey::F32 {
                byte: immediate.get_bytes(),
            },
            Immediate::F64(_) => ImmediateKey::F64 {
                byte: immediate.get_bytes(),
            },
        }
    }
}

impl From<&ImmediateKey> for Immediate {
    fn from(value: &ImmediateKey) -> Self {
        match value {
            ImmediateKey::Int { ty, byte } => match ty {
                ValueType::U8 => Immediate::U8(u64::from_le_bytes(*byte) as u8),
                ValueType::U16 => Immediate::U16(u64::from_le_bytes(*byte) as u16),
                ValueType::U32 => Immediate::U32(u64::from_le_bytes(*byte) as u32),
                ValueType::U64 => Immediate::U64(u64::from_le_bytes(*byte)),
                ValueType::I16 => Immediate::I16(i64::from_le_bytes(*byte) as i16),
                ValueType::I32 => Immediate::I32(i64::from_le_bytes(*byte) as i32),
                ValueType::I64 => Immediate::I64(i64::from_le_bytes(*byte)),
                ValueType::F32 => Immediate::F32(f64::from_le_bytes(*byte) as f32),
                ValueType::F64 => Immediate::F64(f64::from_le_bytes(*byte)),
                ValueType::Mem(_) => unreachable!(),
            },
            ImmediateKey::F32 { byte } => Immediate::F32(f64::from_le_bytes(*byte) as f32),
            ImmediateKey::F64 { byte } => Immediate::F64(f64::from_le_bytes(*byte)),
        }
    }
}

impl From<ImmediateKey> for Immediate {
    fn from(value: ImmediateKey) -> Self {
        (&value).into()
    }
}

// pub fn compute_unary_immi(opcode: OpCode, immi: Immediate) -> Immediate {
//     match opcode {
//         OpCode::Neg => match immi {
//             Immediate::I16(value) => Immediate::I16(-value),
//             Immediate::I32(value) => Immediate::I32(-value),
//             Immediate::I64(value) => Immediate::I64(-value),
//             Immediate::F32(value) => Immediate::F32(-value),
//             Immediate::F64(value) => Immediate::F64(-value),
//             _ => panic!("Neg operation is only supported for signed integers and floating point numbers."),
//         },
//         OpCode::ToU8 => todo!(),
//         OpCode::ToU16 => todo!(),
//         OpCode::ToU32 => todo!(),
//         OpCode::ToU64 => todo!(),
//         OpCode::ToI16 => todo!(),
//         OpCode::ToI32 => todo!(),
//         OpCode::ToI64 => todo!(),
//         OpCode::ToF32 => todo!(),
//         OpCode::ToF64 => todo!(),
//         _ => {
//             panic!("Unsupported opcode for unary operation: {:?}", opcode);
//         }
//     }
// }

// pub fn compute_binary_immi(opcode: OpCode, left: Immediate, right: Immediate) -> Immediate {
//     match opcode {
//         OpCode::Add | OpCode::Addi | OpCode::FAdd => match (left, right) {
//             (Immediate::U8(l), Immediate::U8(r)) => Immediate::U8(l.wrapping_add(r)),
//             (Immediate::U16(l), Immediate::U16(r)) => Immediate::U16(l.wrapping_add(r)),
//             (Immediate::U32(l), Immediate::U32(r)) => Immediate::U32(l.wrapping_add(r)),
//             (Immediate::U64(l), Immediate::U64(r)) => Immediate::U64(l.wrapping_add(r)),
//             (Immediate::I16(l), Immediate::I16(r)) => Immediate::I16(l.wrapping_add(r)),
//             (Immediate::I32(l), Immediate::I32(r)) => Immediate::I32(l.wrapping_add(r)),
//             (Immediate::I64(l), Immediate::I64(r)) => Immediate::I64(l.wrapping_add(r)),
//             (Immediate::F32(l), Immediate::F32(r)) => Immediate::F32(l + r),
//             (Immediate::F64(l), Immediate::F64(r)) => Immediate::F64(l + r),
//             _ => panic!("Immediate type is missmatch."),
//         },
//         OpCode::Sub | OpCode::Subi | OpCode::FSub => match (left, right) {
//             (Immediate::U8(l), Immediate::U8(r)) => Immediate::U8(l.wrapping_sub(r)),
//             (Immediate::U16(l), Immediate::U16(r)) => Immediate::U16(l.wrapping_sub(r)),
//             (Immediate::U32(l), Immediate::U32(r)) => Immediate::U32(l.wrapping_sub(r)),
//             (Immediate::U64(l), Immediate::U64(r)) => Immediate::U64(l.wrapping_sub(r)),
//             (Immediate::I16(l), Immediate::I16(r)) => Immediate::I16(l.wrapping_sub(r)),
//             (Immediate::I32(l), Immediate::I32(r)) => Immediate::I32(l.wrapping_sub(r)),
//             (Immediate::I64(l), Immediate::I64(r)) => Immediate::I64(l.wrapping_sub(r)),
//             (Immediate::F32(l), Immediate::F32(r)) => Immediate::F32(l - r),
//             (Immediate::F64(l), Immediate::F64(r)) => Immediate::F64(l - r),
//             _ => panic!("Immediate type is missmatch."),
//         },
//         OpCode::Mul | OpCode::Muli | OpCode::FMul => match (left, right) {
//             (Immediate::U8(l), Immediate::U8(r)) => Immediate::U8(l.wrapping_mul(r)),
//             (Immediate::U16(l), Immediate::U16(r)) => Immediate::U16(l.wrapping_mul(r)),
//             (Immediate::U32(l), Immediate::U32(r)) => Immediate::U32(l.wrapping_mul(r)),
//             (Immediate::U64(l), Immediate::U64(r)) => Immediate::U64(l.wrapping_mul(r)),
//             (Immediate::I16(l), Immediate::I16(r)) => Immediate::I16(l.wrapping_mul(r)),
//             (Immediate::I32(l), Immediate::I32(r)) => Immediate::I32(l.wrapping_mul(r)),
//             (Immediate::I64(l), Immediate::I64(r)) => Immediate::I64(l.wrapping_mul(r)),
//             (Immediate::F32(l), Immediate::F32(r)) => Immediate::F32(l * r),
//             (Immediate::F64(l), Immediate::F64(r)) => Immediate::F64(l * r),
//             _ => panic!("Immediate type is missmatch."),
//         },
//         OpCode::Divide | OpCode::Dividei | OpCode::FDivide => match (left, right) {
//             (Immediate::U8(l), Immediate::U8(r)) => Immediate::U8(l.wrapping_div(r)),
//             (Immediate::U16(l), Immediate::U16(r)) => Immediate::U16(l.wrapping_div(r)),
//             (Immediate::U32(l), Immediate::U32(r)) => Immediate::U32(l.wrapping_div(r)),
//             (Immediate::U64(l), Immediate::U64(r)) => Immediate::U64(l.wrapping_div(r)),
//             (Immediate::I16(l), Immediate::I16(r)) => Immediate::I16(l.wrapping_div(r)),
//             (Immediate::I32(l), Immediate::I32(r)) => Immediate::I32(l.wrapping_div(r)),
//             (Immediate::I64(l), Immediate::I64(r)) => Immediate::I64(l.wrapping_div(r)),
//             (Immediate::F32(l), Immediate::F32(r)) => Immediate::F32(l / r),
//             (Immediate::F64(l), Immediate::F64(r)) => Immediate::F64(l / r),
//             _ => panic!("Immediate type is missmatch."),
//         },
//         OpCode::Reminder | OpCode::Reminderi | OpCode::FReminder => match (left, right) {
//             (Immediate::U8(l), Immediate::U8(r)) => Immediate::U8(l.wrapping_rem(r)),
//             (Immediate::U16(l), Immediate::U16(r)) => Immediate::U16(l.wrapping_rem(r)),
//             (Immediate::U32(l), Immediate::U32(r)) => Immediate::U32(l.wrapping_rem(r)),
//             (Immediate::U64(l), Immediate::U64(r)) => Immediate::U64(l.wrapping_rem(r)),
//             (Immediate::I16(l), Immediate::I16(r)) => Immediate::I16(l.wrapping_rem(r)),
//             (Immediate::I32(l), Immediate::I32(r)) => Immediate::I32(l.wrapping_rem(r)),
//             (Immediate::I64(l), Immediate::I64(r)) => Immediate::I64(l.wrapping_rem(r)),
//             _ => panic!("Immediate type is missmatch."),
//         },
//         OpCode::BitwiseNot => todo!(),
//         OpCode::BitwiseOR => todo!(),
//         OpCode::BitwiseAnd => todo!(),
//         OpCode::ShiftLeft => todo!(),
//         OpCode::ShiftRight => todo!(),
//         _ => panic!("Unsupported opcode for binary operation: {:?}", opcode),
//     }
// }
