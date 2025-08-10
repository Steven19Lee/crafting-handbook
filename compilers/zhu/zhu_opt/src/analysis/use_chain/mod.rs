use std::collections::{HashMap, HashSet};

use crate::AnalysisPass;
use zhu_ir::entities::function::Function;
use zhu_ir::entities::instruction::{Instruction, InstructionCommon};
use zhu_ir::entities::value::Value;

/// Represent the relationshop of (value, instruction), `use` is a binary relation
/// (value, instruction) is indicate that a `value` is used by an `instruction`.
#[derive(Debug, Clone, PartialEq)]
pub struct UseTable {
    table: HashMap<Value, HashSet<Instruction>>,
}

impl UseTable {
    pub fn new() -> Self {
        Self {
            table: Default::default(),
        }
    }
    /// Get the use set of `value`, which is a set of instructions that use this value.
    pub fn get_use(&self, value: Value) -> Option<&HashSet<Instruction>> {
        self.table.get(&value)
    }
    /// Add relationship that `value` is used by `inst`
    fn add_use(&mut self, value: Value, inst: Instruction) {
        self.table.entry(value).or_default().insert(inst);
    }
}

pub struct UseChainPass;

impl AnalysisPass<UseTable> for UseChainPass {
    fn process(&mut self, func: &Function) -> UseTable {
        // Implementation of the use chain analysis pass
        let mut use_table = UseTable::new();
        self.run(func, &mut use_table);
        use_table
    }
}

impl UseChainPass {
    pub fn new() -> Self {
        Self {}
    }
    fn run(&self, function: &Function, pass: &mut UseTable) {
        for inst in function.insts() {
            let inst_data = function.get_inst_data(inst);
            for operand in inst_data.get_operands() {
                pass.add_use(operand, inst);
            }
        }
    }
}
