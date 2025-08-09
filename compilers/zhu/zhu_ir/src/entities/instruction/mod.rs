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

trait InstructionCommon {
    fn get_operands(&self) -> Vec<Value>;
    fn contain_operand(&self, value: Value) -> bool;
}

impl InstructionData {
    pub fn get_operands(&self) -> Vec<Value> {
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
    pub fn contain_operand(&self, operand: Value) -> bool {
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
    pub fn is_const(&self) -> bool {
        matches!(self, InstructionData::UnaryConst { .. })
    }
    pub fn is_branch(&self) -> bool {
        matches!(self, InstructionData::BrIf { .. } | InstructionData::Jump { .. })
    }
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

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum InstOperandKey {
    Unary(OpCode, Value),
    BinaryI(OpCode, Value, ImmediateKey),
    Binary(OpCode, Value, Value),
    Cmp(OpCode, CmpFlag, Value, Value),
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
    pub fn get_result_value_type(&self, function: &Function) -> ValueType {
        match self {
            InstOperandKey::Unary(op_code, value) => function.value_type(*value).clone(),
            InstOperandKey::BinaryI(op_code, value, immediate_key) => function.value_type(*value).clone(),
            InstOperandKey::Binary(op_code, value, value1) => function.value_type(*value).clone(),
            InstOperandKey::Cmp(op_code, cmp_flag, value, value1) => function.value_type(*value).clone(),
        }
    }
}

impl From<InstOperandKey> for InstructionData {
    fn from(key: InstOperandKey) -> Self {
        match key {
            InstOperandKey::Unary(op_code, value) => InstructionData::Unary { opcode: op_code, value },
            InstOperandKey::BinaryI(op_code, value, imm_key) => InstructionData::BinaryI {
                opcode: op_code,
                value,
                imm: imm_key.into(),
            },
            InstOperandKey::Binary(op_code, value, value1) => InstructionData::Binary {
                opcode: op_code,
                args: [value, value1],
            },
            InstOperandKey::Cmp(op_code, cmp_flag, value, value1) => match op_code {
                OpCode::Fcmp => InstructionData::Fcmp {
                    opcode: op_code,
                    flag: cmp_flag,
                    args: [value, value1],
                },
                OpCode::Icmp => InstructionData::Icmp {
                    opcode: op_code,
                    flag: cmp_flag,
                    args: [value, value1],
                },
                _ => unreachable!(),
            },
        }
    }
}

impl From<&InstOperandKey> for InstructionData {
    fn from(key: &InstOperandKey) -> Self {
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
        match self {
            InstructionData::Unary { opcode, value } => Some(InstOperandKey::Unary(opcode, value)),
            InstructionData::BinaryI { opcode, value, imm } => Some(InstOperandKey::BinaryI(opcode, value, imm.into())),
            InstructionData::Convert { opcode, src } => Some(InstOperandKey::Unary(opcode, src)),
            InstructionData::Binary { opcode, args } => Some(InstOperandKey::Binary(opcode, args[0], args[1])),
            InstructionData::Icmp { opcode, flag, args } => Some(InstOperandKey::Cmp(opcode, flag, args[0], args[1])),
            InstructionData::Fcmp { opcode, flag, args } => Some(InstOperandKey::Cmp(opcode, flag, args[0], args[1])),
            _ => None,
        }
    }
}

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
