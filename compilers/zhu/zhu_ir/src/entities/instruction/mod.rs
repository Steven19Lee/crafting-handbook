use std::collections::HashSet;

use crate::entities::block::Block;
use crate::entities::function::{Function, FunctionRef};
use crate::entities::immediate::{Immediate, ImmediateKey, Offset};
use crate::entities::instruction::opcode::CmpFlag;
use crate::entities::instruction::opcode::OpCode;
use crate::entities::r#type::ValueType;
use crate::entities::value::Value;

use super::constant::Constant;
use super::global_value::GlobalValue;

pub mod opcode;
/// ## Instruction
/// A reference to instruction in a function.
#[derive(Debug, PartialEq, Clone, Eq, Hash, Copy)]
pub struct Instruction(pub u32);
/// Data Entity to represent Instrcution in IR
///
/// Instruction Data is a reference container, for a instruction, it will reference
/// to Value and other data entity in instruction, like immediate or constant.
///
/// This Data entity dose not store fully information for instruction, for example, type
/// of this instruction right hand side or left hand side need to use `Function` instance
/// to get the real data entity.
#[derive(Debug, PartialEq, Clone)]
pub enum InstructionData {
    // Const instruction
    UnaryConst {
        opcode: OpCode,
        constant: Constant,
    },
    Unary {
        opcode: OpCode,
        value: Value,
    },
    Binary {
        opcode: OpCode,
        args: [Value; 2],
    },
    BinaryI {
        opcode: OpCode,
        value: Value,
        imm: Immediate,
    },
    Move {
        opcode: OpCode,
        src: Value,
    },
    Icmp {
        opcode: OpCode,
        flag: CmpFlag,
        args: [Value; 2],
    },
    Fcmp {
        opcode: OpCode,
        flag: CmpFlag,
        args: [Value; 2],
    },
    // function call
    Call {
        opcode: OpCode,
        name: FunctionRef,
        params: Vec<Value>,
    },
    Ret {
        opcode: OpCode,
        value: Option<Value>,
    },
    // data type convert
    Convert {
        opcode: OpCode,
        src: Value,
    },
    // stack related
    StackAlloc {
        opcode: OpCode,
        size: Immediate,
        align: Immediate,
    },
    // memory instruction
    LoadRegister {
        opcode: OpCode,
        base: Value,
        offset: Offset,
    },
    StoreRegister {
        opcode: OpCode,
        base: Value,
        offset: Offset,
        src: Value,
    },
    GlobalLoad {
        opcode: OpCode,
        base: GlobalValue,
        offset: Offset,
    },
    GlobalStore {
        opcode: OpCode,
        base: GlobalValue,
        offset: Offset,
        src: Value,
    },
    // Control instructions
    BrIf {
        opcode: OpCode,
        test: Value,
        conseq: Block,
        alter: Block,
    },
    Jump {
        opcode: OpCode,
        dst: Block,
    },
    // Phi
    Phi {
        opcode: OpCode,
        from: Vec<(Block, Value)>,
    },
    // comment,
    Comment(String),
}
/// Hashable and Euqalable version of `InstructionData` data entity.
///
/// ### Why Need this data entity ?
/// Floating number can not implement totally eq (since NaN), but we will need to compare and
/// equal `InstructionData` for some reason (like GVN or LCM pass), so we will need this data
/// entity to store same information for `InstructionData` and keep hashable.
///
/// ### Caveat
/// Only the InstructionData that
#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum InstOperandKey {
    Unary(OpCode, Value),
    BinaryI(OpCode, Value, ImmediateKey),
    Binary(OpCode, Value, Value),
    Cmp(OpCode, CmpFlag, Value, Value),
}
/// Basic trait to get reference data in Instruction.
pub trait InstructionCommon {
    /// Get operand in Instruction
    fn get_operands(&self) -> Vec<Value>;
    /// Is given operand in Instruction ?
    fn contain_operand(&self, value: Value) -> bool;
}

impl InstructionCommon for &InstructionData {
    fn get_operands(&self) -> Vec<Value> {
        match self {
            InstructionData::UnaryConst { .. } => vec![],
            InstructionData::Unary { value, .. } => vec![value.clone()],
            InstructionData::Binary { args, .. } => args.to_vec(),
            InstructionData::BinaryI { value, .. } => vec![value.clone()],
            InstructionData::Move { src, .. } => vec![src.clone()],
            InstructionData::Icmp { args, .. } | InstructionData::Fcmp { args, .. } => args.to_vec(),
            InstructionData::Call { params, .. } => params.clone(),
            InstructionData::Ret { value, .. } => value.iter().cloned().collect(),
            InstructionData::Convert { src, .. } => vec![src.clone()],
            InstructionData::StackAlloc { .. } => vec![],
            InstructionData::LoadRegister { base, .. } | InstructionData::StoreRegister { base, .. } => {
                vec![base.clone()]
            }
            InstructionData::GlobalLoad { .. } | InstructionData::GlobalStore { .. } => vec![],
            InstructionData::BrIf { test, .. } => vec![test.clone()],
            InstructionData::Jump { .. } => vec![],
            InstructionData::Phi { from, .. } => from.iter().map(|(_, v)| v.clone()).collect(),
            InstructionData::Comment(_) => vec![],
        }
    }
    fn contain_operand(&self, operand: Value) -> bool {
        match self {
            InstructionData::UnaryConst { .. } => false,
            InstructionData::Unary { value, .. } => (*value) == operand,
            InstructionData::Binary { args, .. } => args.iter().any(|arg| *arg == operand),
            InstructionData::BinaryI { value, .. } => *value == operand,
            InstructionData::Move { src, .. } => *src == operand,
            InstructionData::Icmp { args, .. } | InstructionData::Fcmp { args, .. } => {
                args.iter().any(|arg| *arg == operand)
            }
            InstructionData::Call { params, .. } => params.iter().any(|param| *param == operand),
            InstructionData::Ret { value, .. } => value.iter().any(|value| *value == operand),
            InstructionData::Convert { src, .. } => *src == operand,
            InstructionData::StackAlloc { .. } => false,
            InstructionData::LoadRegister { base, .. } | InstructionData::StoreRegister { base, .. } => {
                *base == operand
            }
            InstructionData::GlobalLoad { .. } | InstructionData::GlobalStore { .. } => false,
            InstructionData::BrIf { test, .. } => *test == operand,
            InstructionData::Jump { .. } => false,
            InstructionData::Phi { from, .. } => from.iter().any(|(_, value)| *value == operand),
            InstructionData::Comment(_) => false,
        }
    }
}

impl InstructionCommon for InstructionData {
    fn get_operands(&self) -> Vec<Value> {
        (&self).get_operands()
    }

    fn contain_operand(&self, value: Value) -> bool {
        (&self).contain_operand(value)
    }
}

/// Extra method for InstructionData.
impl InstructionData {
    /// Is Unary Constant Instruction ?
    pub fn is_const(&self) -> bool {
        matches!(self, InstructionData::UnaryConst { .. })
    }
    /// Is Branch instruction (Brif or Jump) ?
    pub fn is_branch(&self) -> bool {
        matches!(self, InstructionData::BrIf { .. } | InstructionData::Jump { .. })
    }
    /// Is instruction will trigger side effect (Memory or global relate) ?
    pub fn has_side_effect(&self) -> bool {
        matches!(
            self,
            InstructionData::BrIf { .. }
                | InstructionData::Jump { .. }
                | InstructionData::Ret { .. }
                | InstructionData::Call { .. }
                | InstructionData::Comment(_)
                | InstructionData::Phi { .. }
                | InstructionData::LoadRegister { .. }
                | InstructionData::GlobalLoad { .. }
                | InstructionData::StoreRegister { .. }
                | InstructionData::GlobalStore { .. }
                | InstructionData::StackAlloc { .. }
        )
    }
}

impl InstOperandKey {
    pub fn contain_operand(&self, operand: Value) -> bool {
        match self {
            InstOperandKey::Unary(_op_code, value) => *value == operand,
            InstOperandKey::Binary(_op_code, value, value1) => *value == operand || *value1 == operand,
            InstOperandKey::BinaryI(_op_code, value, ..) => *value == operand,
            InstOperandKey::Cmp(_op_code, _cmp_flag, value, value1) => *value == operand || *value1 == operand,
        }
    }
    pub fn fmt_key(&self) -> String {
        match self {
            InstOperandKey::Unary(op_code, value) => {
                format!("{} reg{}", op_code, value.0)
            }
            InstOperandKey::BinaryI(op_code, value, imm_key) => {
                let imm: Immediate = imm_key.into();
                format!("{} reg{} {}", op_code, value.0, imm)
            }
            InstOperandKey::Binary(op_code, value, value1) => {
                format!("{} reg{} reg{}", op_code, value.0, value1.0)
            }
            InstOperandKey::Cmp(op_code, cmp_flag, value, value1) => {
                format!("{} {} reg{} reg{}", op_code, cmp_flag, value.0, value1.0)
            }
        }
    }
    ///
    pub fn get_result_value_type(&self, function: &Function) -> ValueType {
        match self {
            InstOperandKey::Unary(op_code, value) => function.value_type(*value).clone(),
            InstOperandKey::BinaryI(op_code, value, immediate_key) => function.value_type(*value).clone(),
            InstOperandKey::Binary(op_code, value, value1) => function.value_type(*value).clone(),
            InstOperandKey::Cmp(op_code, cmp_flag, value, value1) => function.value_type(*value).clone(),
        }
    }
}

impl AsRef<InstOperandKey> for InstOperandKey {
    fn as_ref(&self) -> &InstOperandKey {
        self
    }
}

impl<T> From<T> for InstructionData
where
    T: AsRef<InstOperandKey>,
{
    fn from(key: T) -> Self {
        let key = key.as_ref();
        match key {
            InstOperandKey::Unary(op_code, value) => InstructionData::Unary {
                opcode: *op_code,
                value: *value,
            },
            InstOperandKey::BinaryI(op_code, value, imm_key) => InstructionData::BinaryI {
                opcode: *op_code,
                value: *value,
                imm: imm_key.into(),
            },
            InstOperandKey::Binary(op_code, value, value1) => InstructionData::Binary {
                opcode: *op_code,
                args: [*value, *value1],
            },
            InstOperandKey::Cmp(op_code, cmp_flag, value, value1) => match op_code {
                OpCode::Fcmp => InstructionData::Fcmp {
                    opcode: *op_code,
                    flag: *cmp_flag,
                    args: [*value, *value1],
                },
                OpCode::Icmp => InstructionData::Icmp {
                    opcode: *op_code,
                    flag: *cmp_flag,
                    args: [*value, *value1],
                },
                _ => unreachable!(),
            },
        }
    }
}

impl AsRef<InstructionData> for InstructionData {
    fn as_ref(&self) -> &InstructionData {
        self
    }
}

impl Into<Option<InstOperandKey>> for &InstructionData {
    fn into(self) -> Option<InstOperandKey> {
        match self {
            InstructionData::Unary { opcode, value } => Some(InstOperandKey::Unary(*opcode, *value)),
            InstructionData::BinaryI { opcode, value, imm } => {
                Some(InstOperandKey::BinaryI(*opcode, *value, imm.into()))
            }
            InstructionData::Convert { opcode, src } => Some(InstOperandKey::Unary(*opcode, *src)),
            InstructionData::Binary { opcode, args } => Some(InstOperandKey::Binary(*opcode, args[0], args[1])),
            InstructionData::Icmp { opcode, flag, args } => Some(InstOperandKey::Cmp(*opcode, *flag, args[0], args[1])),
            InstructionData::Fcmp { opcode, flag, args } => Some(InstOperandKey::Cmp(*opcode, *flag, args[0], args[1])),
            _ => None,
        }
    }
}

impl Into<Option<InstOperandKey>> for InstructionData {
    fn into(self) -> Option<InstOperandKey> {
        (&self).into()
    }
}
/// Helper function for convert set of `Instruction` in function to `InstructionKey`
pub fn insts_to_keys(insts: Vec<Instruction>, function: &Function) -> HashSet<InstOperandKey> {
    let mut keys = HashSet::new();
    for inst in insts {
        let inst_data = function.get_inst_data(inst);
        let inst_key: Option<InstOperandKey> = inst_data.clone().into();
        if let Some(key) = inst_key {
            keys.insert(key);
        }
    }
    keys
}
