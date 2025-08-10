use zhu_ir::entities::function::Function;
use zhu_ir::entities::immediate::Immediate;
use zhu_ir::entities::instruction::{Instruction, InstructionData};
use zhu_ir::entities::r#type::ValueType;
use zhu_ir::entities::value::ValueData;

#[derive(Debug, Clone, PartialEq)]
pub enum Lattice {
    Top,
    Bottom,
    Value(Immediate),
}

impl Lattice {
    /// Create a lattice from instruction, since it involves instruction data and result value.
    /// so we need to pass the function to get the instruction data and value data.
    pub fn from_inst(inst: &Instruction, function: &Function) -> Self {
        let inst_data = function.get_inst_data(*inst);
        match *inst_data {
            // For const instruction, according to constant type, predocue the immediate value.
            InstructionData::UnaryConst { constant, .. } => {
                let result = function.get_inst_result(*inst).unwrap();
                let result_data = function.get_value_data(result);
                let constant_data = function.constants.get(&constant).unwrap();
                match result_data {
                    // If the result is a constant, we can use its immediate value
                    ValueData::Inst { ty, .. } => match *ty {
                        ValueType::U8 => {
                            let byte = [constant_data.bytes[0..1][0]];
                            println!("{:?} {:?}", constant_data.bytes, u8::from_be_bytes([0x01]));
                            Lattice::Value(Immediate::U8(u8::from_be_bytes([0x01])))
                        }
                        ValueType::U16 => todo!(),
                        ValueType::U32 => todo!(),
                        ValueType::U64 => todo!(),
                        ValueType::I16 => todo!(),
                        ValueType::I32 => todo!(),
                        ValueType::I64 => todo!(),
                        ValueType::F32 => todo!(),
                        ValueType::F64 => todo!(),
                        ValueType::Mem(mem_type) => unreachable!(),
                    },
                    // result must from a unary const instruction.
                    ValueData::Param { .. } => unreachable!(),
                }
            }
            // For Most of non-side-effect compute instruction, using optimistic value.
            InstructionData::Unary { .. }
            | InstructionData::Binary { .. }
            | InstructionData::BinaryI { .. }
            | InstructionData::Move { .. }
            | InstructionData::Icmp { .. }
            | InstructionData::Fcmp { .. }
            | InstructionData::Convert { .. } => Lattice::Top,
            // For instruction that has side effect or interact with external state, using pessimistic value.
            InstructionData::Call { .. }
            | InstructionData::StackAlloc { .. }
            | InstructionData::LoadRegister { .. }
            | InstructionData::StoreRegister { .. }
            | InstructionData::GlobalLoad { .. }
            | InstructionData::GlobalStore { .. } => Lattice::Bottom,
            //  Branch relative instruction dose not have result value, using optimistic value
            InstructionData::BrIf { .. } | InstructionData::Jump { .. } => Lattice::Top,
            // For Ret instruction, always using optimistic value
            InstructionData::Ret { .. } => Lattice::Top,
            // For Phi instruction, always using optimistic value
            InstructionData::Phi { .. } => Lattice::Top,
            // comment
            InstructionData::Comment(_) => Lattice::Bottom,
        }
    }
}
