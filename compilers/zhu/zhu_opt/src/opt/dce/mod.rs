pub mod is_critical;
pub mod post_domtree;
use std::collections::HashSet;

use crate::OptiPass;
use post_domtree::PostDomTree;
use zhu_ir::entities::block::Block;
use zhu_ir::entities::function::Function;
use zhu_ir::entities::instruction::opcode::OpCode;
use zhu_ir::entities::instruction::{Instruction, InstructionCommon, InstructionData};
use zhu_ir::entities::value::ValueData;

use is_critical::is_critical_inst;

pub fn dce_pass(func: &mut Function, post_dom: &PostDomTree) {
    let mut dce = DeadCodeEliminationPass::new(post_dom);
    dce.process(func);
}

pub struct DeadCodeEliminationPass<'a> {
    post_dom: &'a PostDomTree,
    mark_insts: HashSet<Instruction>,
    mark_blocks: HashSet<Block>,
}

impl<'a> OptiPass for DeadCodeEliminationPass<'a> {
    fn process(&mut self, func: &mut Function) {
        self.mark_pass(func);
        self.swap_pass(func);
    }
}

impl<'a> DeadCodeEliminationPass<'a> {
    pub fn new(post_dom: &'a PostDomTree) -> Self {
        Self {
            post_dom,
            mark_blocks: Default::default(),
            mark_insts: Default::default(),
        }
    }
}

impl<'a> DeadCodeEliminationPass<'a> {
    /// Fill mark set with insts.
    fn mark_pass(&mut self, func: &Function) {
        // mark critial insts
        let mut worklist = Vec::new();
        for inst in &func.insts() {
            let block = func.get_block_of_inst(*inst);
            let inst_data = func.get_inst_data(*inst);
            if is_critical_inst(inst_data) {
                self.mark_inst_and_block(*inst, block);
                worklist.push(inst.clone());
            }
        }
        // for every critical inst,
        // - mark all it's relation insts.
        // - for data deps blocks, mark last branch inst.
        while worklist.len() > 0 {
            let inst = worklist.pop().unwrap();
            let block = func.get_block_of_inst(inst);
            let operands = func.get_inst_data(inst).get_operands();
            for operand in operands {
                let operand_data = func.get_value_data(operand);
                if let ValueData::Inst { inst: def_inst, .. } = operand_data {
                    if !self.mark_insts.contains(def_inst) {
                        self.mark_inst_and_block(*def_inst, block);
                        worklist.push(def_inst.clone());
                    }
                }
            }
            for df in self.post_dom.post_df(block) {
                let last_inst = func.layout.get_last_inst(*df);
                if !self.mark_insts.contains(&last_inst) {
                    self.mark_inst_and_block(last_inst, *df);
                    worklist.push(last_inst);
                }
            }
        }
    }
    /// Using mark set to remove inst.
    fn swap_pass(&mut self, func: &mut Function) {
        for inst in func.insts() {
            if !self.mark_insts.contains(&inst) {
                let inst_data = func.get_inst_data(inst);
                if inst_data.is_branch() {
                    self.rewrite_branch_inst_to_closet_mark_post_dom(inst, func);
                } else {
                    func.remove_inst(inst);
                }
            }
        }
    }
}

impl<'a> DeadCodeEliminationPass<'a> {
    fn mark_inst_and_block(&mut self, inst: Instruction, block: Block) {
        self.mark_insts.insert(inst);
        self.mark_blocks.insert(block);
    }
    fn rewrite_branch_inst_to_closet_mark_post_dom(&self, inst: Instruction, func: &mut Function) {
        let block = func.get_block_of_inst(inst);
        let the_closet_mark_post_dominator = {
            let mut runner = self.post_dom.post_idom(block).unwrap();
            while !self.mark_blocks.contains(&runner) {
                runner = self.post_dom.post_idom(runner).unwrap();
            }
            runner
        };
        func.replace_inst(
            inst,
            InstructionData::Jump {
                opcode: OpCode::Jump,
                dst: the_closet_mark_post_dominator,
            },
        );
    }
}
