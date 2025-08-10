pub mod lattices;

use crate::analysis::use_chain::UseTable;
use crate::OptiPass;
use lattices::Lattice;
use std::collections::HashMap;
use zhu_ir::entities::constant::ConstantData;
use zhu_ir::entities::function::Function;
use zhu_ir::entities::immediate::Immediate;
use zhu_ir::entities::instruction::{opcode::OpCode, Instruction, InstructionData};
use zhu_ir::entities::value::{Value, ValueData};

pub fn sscp_pass<'a>(function: &mut Function, use_table: &'a UseTable) {
    let mut pass = SSCPPass::new(use_table);
    pass.process(function);
}

/// Simple Sparse
pub struct SSCPPass<'a> {
    use_table: &'a UseTable,
    lattices: HashMap<Value, Lattice>,
}
impl<'a> OptiPass for SSCPPass<'a> {
    fn process(&mut self, function: &mut Function) {
        let work_list = self.init_stage(function);
        self.propagate_stage(function, work_list);
        self.rewrite_stage(function);
    }
}
/// Implemenation of Simple Sparse Constant Propagation (SSCP) pass.
impl<'a> SSCPPass<'a> {
    pub fn new(use_table: &'a UseTable) -> Self {
        Self {
            use_table,
            lattices: HashMap::new(),
        }
    }

    fn init_stage(&mut self, function: &Function) -> Vec<Value> {
        let mut work_list: Vec<Value> = Vec::new();
        for (value, value_data) in &function.entities.values {
            match value_data {
                ValueData::Inst { inst, .. } => {
                    let lattice_element = Lattice::from_inst(inst, function);
                    if lattice_element != Lattice::Top {
                        work_list.push(*value);
                    }
                    self.lattices.insert(*value, Lattice::from_inst(inst, function));
                }
                ValueData::Param { .. } => {
                    // Q: Should we treat parameters as top or bottom?
                    self.lattices.insert(*value, Lattice::Bottom);
                }
            }
        }
        work_list
    }
    fn propagate_stage(&mut self, function: &Function, mut work_list: Vec<Value>) {
        while work_list.len() > 0 {
            let value = work_list.pop().unwrap();
            for use_inst in self.use_table.get_use(value).unwrap() {
                if let Some(result) = function.get_inst_result(*use_inst) {
                    let original_lattice = self.lattices.get(&result).unwrap();
                    let next_lattice = self.compute_lattice_over_inst(*use_inst, function);
                    if *original_lattice != next_lattice {
                        self.lattices.insert(result, next_lattice);
                        work_list.push(result);
                    }
                }
            }
        }
    }
    fn rewrite_stage(&mut self, function: &mut Function) {
        for (value, lattice) in &self.lattices {
            if let Lattice::Value(immi) = lattice {
                let def_inst = if let ValueData::Inst { inst, .. } = function.get_value_data(*value) {
                    *inst
                } else {
                    unreachable!()
                };
                // skip rewrite if it is a constant instruction.
                let def_inst_data = function.get_inst_data(def_inst);
                if def_inst_data.is_const() {
                    continue;
                }
                let inst_data = convert_immi_to_const_inst(immi.clone(), function);
                let const_inst = function.entities.create_inst(inst_data);
                let result = function.entities.create_value(ValueData::Inst {
                    inst: const_inst,
                    ty: immi.get_value_type(),
                });
                function.entities.mark_inst_result(result, const_inst);
                function.layout.insert_inst_before(const_inst, def_inst);
                function.replace_inst(
                    def_inst,
                    InstructionData::Move {
                        opcode: OpCode::Mov,
                        src: result,
                    },
                );
            }
        }
    }
    fn compute_lattice_over_inst(&self, inst: Instruction, function: &Function) -> Lattice {
        let inst_data = function.get_inst_data(inst);
        match inst_data {
            // Move instruction, just copy the lattice from source
            InstructionData::Move { src, .. } => self.lattices.get(src).unwrap().clone(),
            InstructionData::Unary { opcode, value } => match self.lattices.get(value).unwrap() {
                Lattice::Bottom => Lattice::Bottom,
                Lattice::Top => Lattice::Top,
                Lattice::Value(immi) => Lattice::Value(Immediate::compute_unary_immi(*opcode, immi.clone())),
            },
            InstructionData::Convert { opcode, src } => match self.lattices.get(src).unwrap() {
                Lattice::Bottom => Lattice::Bottom,
                Lattice::Top => Lattice::Top,
                Lattice::Value(immi) => Lattice::Value(Immediate::compute_unary_immi(*opcode, immi.clone())),
            },
            InstructionData::BinaryI { opcode, value, imm } => match self.lattices.get(value).unwrap() {
                Lattice::Bottom => Lattice::Bottom,
                Lattice::Top => Lattice::Top,
                Lattice::Value(arg_immi) => {
                    Lattice::Value(Immediate::compute_binary_immi(*opcode, arg_immi.clone(), imm.clone()))
                }
            },
            InstructionData::Binary { opcode, args } => {
                let arg1_lattice = self.lattices.get(&args[0]).unwrap();
                let arg2_lattice = self.lattices.get(&args[1]).unwrap();
                match (arg1_lattice, arg2_lattice) {
                    (Lattice::Bottom, _) | (_, Lattice::Bottom) => Lattice::Bottom,
                    (Lattice::Top, _) | (_, Lattice::Top) => Lattice::Top,
                    (Lattice::Value(immi1), Lattice::Value(immi2)) => {
                        Lattice::Value(Immediate::compute_binary_immi(*opcode, immi1.clone(), immi2.clone()))
                    }
                }
            }
            // Most of instruction is same as init value, including
            // - unary const instruction
            // - branch relative instruction
            // - side effect instruction
            // - conditional instruction
            // - comment instruction
            // - phi instruction
            // - call instruction
            // - ret instruction
            _ => Lattice::from_inst(&inst, function),
        }
    }
}

fn convert_immi_to_const_inst(immi: Immediate, function: &mut Function) -> InstructionData {
    let bytes = immi.get_bytes();
    let constant = function.create_constant(ConstantData { bytes: bytes.to_vec() });
    let opcode = match immi {
        Immediate::U8(_) | Immediate::U16(_) | Immediate::U32(_) | Immediate::U64(_) => OpCode::Uconst,
        Immediate::I16(_) | Immediate::I32(_) | Immediate::I64(_) => OpCode::Iconst,
        Immediate::F32(_) | Immediate::F64(_) => OpCode::Fconst,
    };

    InstructionData::UnaryConst {
        opcode,
        constant: constant,
    }
}
