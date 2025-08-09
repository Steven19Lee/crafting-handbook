use crate::entities::function::Function;
use crate::entities::instruction::{Instruction, InstructionData};
use crate::entities::module::Module;

use crate::entities::value::ValueData;
use crate::formatter::Formatter;

impl Formatter {
    /// Format instructions, need module since we need to format external name
    pub fn fmt_inst(&self, inst: Instruction, function: &Function, module: &Module) -> String {
        let inst_data = function.entities.insts.get(&inst).unwrap();
        let inst_result = function.entities.insts_result.get(&inst);

        match inst_data {
            InstructionData::UnaryConst { opcode, constant } => {
                let value_type = function.value_type(inst_result.unwrap().clone()).clone();
                let constant_data = function.constants.get(constant).unwrap();
                format!(
                    "reg{} = {} {} [{}]",
                    inst_result.unwrap().0,
                    opcode,
                    self.fmt_value_type(&value_type, function),
                    constant_data
                )
            }
            InstructionData::Unary { opcode, value } => {
                format!("reg{} = {} reg{}", inst_result.unwrap().0, opcode, value.0)
            }
            InstructionData::Binary { opcode, args } => {
                format!(
                    "reg{} = {} reg{} reg{}",
                    inst_result.unwrap().0,
                    opcode,
                    args[0].0,
                    args[1].0
                )
            }
            InstructionData::BinaryI { opcode, value, imm } => {
                format!("reg{} = {} reg{} {}", inst_result.unwrap().0, opcode, value.0, imm)
            }
            InstructionData::Move { opcode, src } => {
                format!("reg{} = {} reg{}", inst_result.unwrap().0, opcode, src.0)
            }
            InstructionData::Icmp { opcode, flag, args } | InstructionData::Fcmp { opcode, flag, args } => {
                format!(
                    "reg{} = {} {} reg{} reg{}",
                    inst_result.unwrap().0,
                    opcode,
                    flag,
                    args[0].0,
                    args[1].0
                )
            }
            InstructionData::Call { opcode, name, params } => {
                let func_sign = function.external_funcs.get(name).unwrap();
                let mut param_string = String::new();
                let mut index = 0;
                for param in params {
                    if index == 0 {
                        param_string.push_str(format!("reg{}", param.0).as_str());
                    } else {
                        param_string.push_str(format!(", reg{}", param.0).as_str());
                    }
                    index += 1;
                }
                let result = match inst_result {
                    Some(reg) => format!("reg{} = ", reg.0),
                    None => String::new(),
                };
                format!(
                    "{}{} func {}({})",
                    result,
                    opcode,
                    self.fmt_external_name(&func_sign.name, module),
                    param_string
                )
            }
            InstructionData::Ret { opcode, value } => {
                if let Some(val) = value {
                    format!("{} reg{}", opcode, val.0)
                } else {
                    format!("{}", opcode)
                }
            }
            InstructionData::Convert { opcode, src } => {
                format!("reg{} = {} reg{}", inst_result.unwrap().0, opcode, src.0)
            }
            InstructionData::StackAlloc { opcode, size, align } => {
                let inst_result_value = inst_result.unwrap();
                let ty_from_inst = match function.entities.values.get(inst_result_value).unwrap() {
                    ValueData::Inst { ty, .. } => ty,
                    _ => panic!(),
                };
                format!(
                    "reg{} = {} {}, size {}, align {}",
                    inst_result.unwrap().0,
                    opcode,
                    self.fmt_value_type(ty_from_inst, function),
                    size,
                    align
                )
            }
            InstructionData::LoadRegister { opcode, base, offset } => {
                let inst_result_value = inst_result.unwrap();
                let ty_from_inst = match function.entities.values.get(inst_result_value).unwrap() {
                    ValueData::Inst { ty, .. } => ty,
                    _ => panic!(),
                };
                format!(
                    "reg{} = {} {} [reg{}, {}]",
                    inst_result.unwrap().0,
                    opcode,
                    self.fmt_value_type(ty_from_inst, function),
                    base.0,
                    offset.0
                )
            }
            InstructionData::StoreRegister {
                opcode,
                base,
                offset,
                src,
            } => {
                format!("{} reg{} [reg{}, {}]", opcode, src.0, base.0, offset.0)
            }
            InstructionData::GlobalLoad { opcode, base, offset } => {
                let inst_result_value = inst_result.unwrap();
                let ty_from_inst = match function.entities.values.get(inst_result_value).unwrap() {
                    ValueData::Inst { ty, .. } => ty,
                    _ => panic!(),
                };
                format!(
                    "reg{} = {} {} [greg{}, {}]",
                    inst_result.unwrap().0,
                    opcode,
                    self.fmt_value_type(ty_from_inst, function),
                    base.0,
                    offset.0
                )
            }
            InstructionData::GlobalStore {
                opcode,
                base,
                offset,
                src,
            } => {
                format!("{} reg{} [greg{}, {}]", opcode, src.0, base.0, offset.0)
            }
            InstructionData::BrIf {
                opcode,
                test,
                conseq,
                alter,
            } => {
                format!("{} reg{} block{} block{}", opcode, test.0, conseq.0, alter.0)
            }
            InstructionData::Jump { opcode, dst } => {
                format!("{} block{}", opcode, dst.0)
            }
            InstructionData::Phi { opcode, from } => {
                let mut phi_string = String::new();
                let mut index = 0;
                let mut phi_source = from.clone();
                phi_source.sort_by_key(|tuple| tuple.0 .0);
                for (block, value) in phi_source {
                    if index == 0 {
                        phi_string.push_str(format!("block{} reg{}", block.0, value.0).as_str());
                    } else {
                        phi_string.push_str(format!(", block{} reg{}", block.0, value.0).as_str());
                    }
                    index += 1;
                }
                format!("reg{} = {} [{}]", inst_result.unwrap().0, opcode, phi_string)
            }
            InstructionData::Comment(s) => {
                format!(";;{}", s)
            }
        }
    }
}
