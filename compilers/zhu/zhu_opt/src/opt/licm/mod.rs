pub mod natural_loop;

use std::collections::HashSet;

use crate::analysis::cfg::ControlFlowGraph;
use crate::analysis::domtree::DomTree;
use crate::analysis::rpo::RevresePostOrder;
use crate::OptiPass;
use natural_loop::NaturalLoop;
use zhu_ir::builder::FunctionBuilder;
use zhu_ir::entities::block::{Block, BlockData};
use zhu_ir::entities::function::Function;
use zhu_ir::entities::instruction::{Instruction, InstructionCommon};
use zhu_ir::entities::util::set_operation::intersection_sets;
use zhu_ir::entities::value::ValueData;

/// Perform Loop Invariant Code Motion (LICM) optimization on the function.
pub fn licm_pass(
    func: &mut Function,
    cfg: &ControlFlowGraph,
    dom: &DomTree,
    rpo: &RevresePostOrder,
    natural_loops: &Vec<NaturalLoop>,
) {
    let mut licm = LoopInvariantCodeMotion::new(cfg, dom, rpo, natural_loops);
    licm.process(func);
}
/// Implementation of Loop Invariant Code Motion (LICM) optimization pass.
pub struct LoopInvariantCodeMotion<'a> {
    pub cfg: &'a ControlFlowGraph,
    pub rpo: &'a RevresePostOrder,
    pub dom: &'a DomTree,
    pub natural_loops: &'a Vec<NaturalLoop>,
}

impl<'a> OptiPass for LoopInvariantCodeMotion<'a> {
    fn process(&mut self, func: &mut Function) {
        for natural_loop in self.natural_loops {
            // Find loop invariants
            let loop_invariants = self.find_loop_invariants(func, natural_loop);
            // Find code mentional loop invariants
            let code_mentional_loop_invariants =
                self.find_code_moitionable_loop_invariants(func, natural_loop, loop_invariants);
            // Remove loop invariants
            self.motion_loop_invariants(func, natural_loop, code_mentional_loop_invariants);
        }
    }
}

impl<'a> LoopInvariantCodeMotion<'a> {
    pub fn new(
        cfg: &'a ControlFlowGraph,
        dom: &'a DomTree,
        rpo: &'a RevresePostOrder,
        natural_loops: &'a Vec<NaturalLoop>,
    ) -> Self {
        Self {
            cfg,
            rpo,
            dom,
            natural_loops,
        }
    }
    /// A instruction is Loop invariant if and only if all of it's operands
    ///
    /// - defined outside the loop (base condition)
    /// - also a loop invariant (recursive condition)
    ///     - A const instruction is also a loop invariant
    ///
    /// If inst A using a loop invariant inst B and C as operand, it's order in return
    /// vec will after B and C, so return vec will be topological order of loop invariants.
    fn find_loop_invariants(&self, func: &Function, natural_loop: &NaturalLoop) -> Vec<Instruction> {
        let mut loop_invariants = Vec::new();
        let mut loop_invariants_in_value = HashSet::new();
        for block in self
            .rpo
            .sort_blocks_in_rpo(natural_loop.blocks.iter().map(|b| b.clone()).collect())
        {
            for inst in &func.get_insts_of_block(block) {
                let inst_data = func.get_inst_data(*inst);
                let oprands = inst_data.get_operands();
                if (oprands.len() != 0
                    && oprands.iter().all(|operand| {
                        let operand_data = func.get_value_data(*operand);
                        match operand_data {
                            ValueData::Inst { inst, .. } => {
                                !natural_loop.blocks.contains(&func.get_block_of_inst(*inst))
                                    || loop_invariants_in_value.contains(operand)
                            }
                            ValueData::Param { .. } => true,
                        }
                    }))
                    || inst_data.is_const()
                {
                    let inst_result = func.get_inst_result(inst.clone());
                    if let Some(result) = inst_result {
                        loop_invariants_in_value.insert(result);
                        loop_invariants.push(inst.clone());
                    }
                }
            }
        }
        loop_invariants
    }
    /// A loop invariant is mentionable if and only if
    ///
    /// - it dominates all exits of the loop
    /// - satidfies SSA property
    fn find_code_moitionable_loop_invariants(
        &self,
        func: &Function,
        natural_loop: &NaturalLoop,
        loop_invariants: Vec<Instruction>,
    ) -> Vec<Instruction> {
        let mut code_mentional_loop_invariants = Vec::new();
        let dominators_of_each_exit = natural_loop
            .exits
            .iter()
            .map(|exit| self.dom.dom(*exit))
            .collect::<Vec<&HashSet<Block>>>();
        let blocks_dominate_all_exis = intersection_sets(dominators_of_each_exit);
        for inst in loop_invariants {
            // get block of inst
            let block = func.get_block_of_inst(inst);
            // Is block dominate all exist ?
            if blocks_dominate_all_exis.contains(&block) {
                code_mentional_loop_invariants.push(inst);
            };
        }
        code_mentional_loop_invariants
    }
    /// Remove loop invariants from the function, seperate to make
    /// borrow checker happy, for
    fn motion_loop_invariants(
        &self,
        func: &mut Function,
        natural_loop: &NaturalLoop,
        loop_invariants: Vec<Instruction>,
    ) {
        // create preheader block in function
        if loop_invariants.len() == 0 {
            return;
        }
        let preheader = func.create_and_insert_block_before(BlockData::new(), natural_loop.header);
        // move loop invariants to preheader block, connect preheader to header
        for inst in loop_invariants {
            func.remove_inst(inst);
            func.append_inst(inst, preheader);
        }
        let mut builder = FunctionBuilder::new(func);
        builder.switch_to_block(preheader);
        builder.jump_inst(natural_loop.header.clone());
        // mutate predecessors of header to connect to preheader
        let predecessors_of_header = self.cfg.get_predecessors(&natural_loop.header);
        for predecessor in predecessors_of_header {
            if *predecessor == natural_loop.tail {
                continue;
            }
            let last_inst = func.layout.get_last_inst(*predecessor);
            let last_inst_data = func.entities.get_inst_data_mut(last_inst);
            match last_inst_data {
                zhu_ir::entities::instruction::InstructionData::Jump { dst, .. } => {
                    if (*dst).0 == natural_loop.header.0 {
                        *dst = preheader.clone();
                    }
                }
                zhu_ir::entities::instruction::InstructionData::BrIf { conseq, alter, .. } => {
                    if *conseq == natural_loop.header {
                        *conseq = preheader.clone();
                    }
                    if *alter == natural_loop.header {
                        *alter = preheader.clone();
                    }
                }
                _ => {}
            }
        }
    }
}
