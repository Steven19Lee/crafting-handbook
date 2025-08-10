use std::collections::{HashMap, HashSet};

use crate::analysis::cfg::ControlFlowGraph;
use crate::opt::lcm::earliest::EarliestExpression;
use crate::opt::lcm::postponable_expr::PostponableExpression;
use crate::{get_table_header, AnalysisPass, FormatTable};
use zhu_ir::entities::block::Block;
use zhu_ir::entities::function::Function;
use zhu_ir::entities::instruction::{insts_to_keys, InstOperandKey};
use zhu_ir::entities::module::Module;
use zhu_ir::entities::util::set_operation::{intersection_sets, union_sets};

pub fn later_expression_anaylsis(
    function: &Function,
    cfg: &ControlFlowGraph,
    earliest: &EarliestExpression,
    postponable: &PostponableExpression,
) -> LaterExpression {
    let mut pass = LaterExpressionPass::new(cfg, earliest, postponable);
    pass.process(function)
}

/// Later expression is defined as
/// ```text
/// later(b) = (
///   earliest(b) || postponeable.in(b)
/// ) && (
///   expression_use(b) ||
///   not (
///     intersection of each successor's
///     (earliest(b) || postponeable.in(b))
///   )
/// )
/// ```
pub struct LaterExpression {
    later: HashMap<Block, HashSet<InstOperandKey>>,
    insertable: HashMap<Block, HashSet<InstOperandKey>>,
}

impl LaterExpression {
    pub fn new() -> Self {
        Self {
            later: Default::default(),
            insertable: Default::default(),
        }
    }
    pub fn get_later(&self, block: Block) -> &HashSet<InstOperandKey> {
        self.later.get(&block).unwrap()
    }
}

impl FormatTable for LaterExpression {
    fn format_table(&self, func: &Function, _module: &Module) -> String {
        let mut format_string = get_table_header("Later Expression");
        for block in func.blocks() {
            format_string.push_str(&format!("Block{}:\n", block.0));
            for key in self.later.get(&block).unwrap() {
                format_string.push_str(&format!("\t{}\n", key.fmt_key()));
            }
        }
        format_string
    }
}
/// Pass to create later expression
pub struct LaterExpressionPass<'a> {
    cfg: &'a ControlFlowGraph,
    earliest: &'a EarliestExpression,
    postponable: &'a PostponableExpression,
}

impl<'a> AnalysisPass<LaterExpression> for LaterExpressionPass<'a> {
    fn process(&mut self, function: &Function) -> LaterExpression {
        let mut pass = LaterExpression::new();
        self.run(function, &mut pass);
        pass
    }
}

impl<'a> LaterExpressionPass<'a> {
    pub fn new(
        cfg: &'a ControlFlowGraph,
        earliest: &'a EarliestExpression,
        postponable: &'a PostponableExpression,
    ) -> Self {
        Self {
            cfg,
            earliest,
            postponable,
        }
    }
    /// Entry point for later expression
    fn run(&self, function: &Function, pass: &mut LaterExpression) {
        self.compute_block_set(function, pass);
        self.iter_flow_equation(function, pass);
    }
    /// Compute `earliest(b) || post_ponable.in(b)` for each block.
    /// There we call this set as `insertable`.
    fn compute_block_set(&self, function: &Function, pass: &mut LaterExpression) {
        for block in function.blocks() {
            let insertable = union_sets(vec![
                self.earliest.get_earliest(block),
                self.postponable.get_postponable_in(block),
            ]);
            pass.insertable.insert(block, insertable);
        }
    }
    /// Run flow equation of later expression definition.
    fn iter_flow_equation(&self, function: &Function, pass: &mut LaterExpression) {
        let all_inst_key_set = insts_to_keys(function.insts(), function);
        for block in function.blocks() {
            let insertable_of_block = pass.insertable.get(&block).unwrap();
            let downward_expression_of_block = self.postponable.get_downward_exposed_expr(block);
            let successor_insertable = intersection_sets(
                self.cfg
                    .get_successors(&block)
                    .iter()
                    .map(|successor| pass.insertable.get(successor).unwrap())
                    .collect(),
            );
            let mut successor_not_insertable = all_inst_key_set.clone();
            successor_not_insertable.retain(|key| !successor_insertable.contains(key));
            let later_set = intersection_sets(vec![
                insertable_of_block,
                &union_sets(vec![downward_expression_of_block, &successor_not_insertable]),
            ]);
            pass.later.insert(block, later_set);
        }
    }
}
