use std::collections::{HashMap, HashSet};

use crate::analysis::cfg::ControlFlowGraph;
use crate::analysis::rpo::RevresePostOrder;
use crate::AnalysisPass;
use zhu_ir::entities::block::Block;
use zhu_ir::entities::function::Function;
use zhu_ir::entities::instruction::Instruction;
use zhu_ir::entities::util::set_operation::{intersection_sets, union_sets};

pub fn available_expression_analysis(
    func: &Function,
    cfg: &ControlFlowGraph,
    rpo: &RevresePostOrder,
) -> AvailableExpression {
    let mut pass = AvailableExpressionPass::new(cfg, rpo);
    pass.process(func)
}
pub struct AvailableExpression {
    downward_exposed_expr: HashMap<Block, HashSet<Instruction>>,
    avail_expr_in: HashMap<Block, HashSet<Instruction>>,
    avail_expr_out: HashMap<Block, HashSet<Instruction>>,
}

impl AvailableExpression {
    pub fn new() -> Self {
        Self {
            downward_exposed_expr: Default::default(),
            avail_expr_in: Default::default(),
            avail_expr_out: Default::default(),
        }
    }
    /// Get availableOut set of given block
    pub fn get_available_out(&self, block: Block) -> &HashSet<Instruction> {
        self.avail_expr_out.get(&block).unwrap()
    }
}

pub struct AvailableExpressionPass<'a> {
    rpo: &'a RevresePostOrder,
    cfg: &'a ControlFlowGraph,
}

impl<'a> AnalysisPass<AvailableExpression> for AvailableExpressionPass<'a> {
    fn process(&mut self, func: &Function) -> AvailableExpression {
        let mut exprs = AvailableExpression::new();
        self.run(func, &mut exprs);
        exprs
    }
}

impl<'a> AvailableExpressionPass<'a> {
    /// Create a new available expression pass.
    pub fn new(cfg: &'a ControlFlowGraph, rpo: &'a RevresePostOrder) -> Self {
        Self { cfg, rpo }
    }
    /// Add only needed instruction, skip all side effect instruction.
    fn filter_inst(inst: Instruction, func: &Function) -> bool {
        let inst_data = func.get_inst_data(inst);
        inst_data.has_side_effect()
    }
    /// Run fixed point iterative algorithm as forward data flow anaylsis.
    /// ```text
    /// avail_in = intersection of predecessors avail_out
    /// avail_out = avail_in & down_expose_expr
    /// ```
    fn run(&mut self, function: &Function, exprs: &mut AvailableExpression) {
        self.init_sets(function, exprs);
        self.compute_single_block_set(function, exprs);
        self.iter_flow_equation(exprs);
    }
    /// Init in and out set as forward data flow
    fn init_sets(&mut self, function: &Function, exprs: &mut AvailableExpression) {
        let all_insts: HashSet<Instruction> = function
            .insts()
            .into_iter()
            .filter(|inst| Self::filter_inst(*inst, function))
            .collect();
        let entry = self.cfg.get_entry();
        for block in function.blocks() {
            if block == entry {
                exprs.avail_expr_in.insert(block, Default::default());
            } else {
                exprs.avail_expr_in.insert(block, HashSet::from(all_insts.clone()));
            };
            exprs.avail_expr_out.insert(block, Default::default());
        }
    }
    /// Get downward-exposed expression and expression kill set of block
    /// - Since our IR is SSA, it imply that:
    ///   1. there is no expression kill set, since we will not re-defined
    ///      a operand in SSA
    ///   2. every expression is downward-exposed, since expression will not
    ///      be kill in same block.
    fn compute_single_block_set(&mut self, function: &Function, exprs: &mut AvailableExpression) {
        for block in function.blocks() {
            exprs.downward_exposed_expr.insert(
                block,
                function
                    .get_insts_of_block(block)
                    .into_iter()
                    .filter(|inst| Self::filter_inst(*inst, function))
                    .collect(),
            );
        }
    }
    /// Iterative forward data flow algorithm of available expression.
    fn iter_flow_equation(&mut self, exprs: &mut AvailableExpression) {
        let mut is_changed = true;
        let blocks_in_rpo = self.rpo.get_blocks_in_rpo();
        while is_changed {
            is_changed = false;
            for block in &blocks_in_rpo {
                // Compute In and Out set
                let in_set = {
                    let predecessors = self.cfg.get_predecessors(block);
                    let out_sets_of_predecessors: Vec<&HashSet<Instruction>> = predecessors
                        .iter()
                        .map(|predecessor| exprs.avail_expr_out.get(predecessor).unwrap())
                        .collect();
                    intersection_sets(out_sets_of_predecessors)
                };
                let next_out_set = union_sets(vec![&in_set, exprs.downward_exposed_expr.get(block).unwrap()]);
                let current_out_set = exprs.avail_expr_out.get(block).unwrap();
                // Insert in and out set
                exprs.avail_expr_in.insert(*block, in_set);
                if next_out_set != *current_out_set {
                    is_changed = true;
                    exprs.avail_expr_out.insert(*block, next_out_set);
                }
            }
        }
    }
}
