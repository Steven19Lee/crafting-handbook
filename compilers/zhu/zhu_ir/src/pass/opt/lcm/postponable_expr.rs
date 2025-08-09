use std::collections::{HashMap, HashSet};

use crate::entities::block::Block;
use crate::entities::function::Function;
use crate::entities::instruction::{insts_to_keys, InstOperandKey};
use crate::entities::module::Module;
use crate::entities::util::set_operation::{intersection_sets, union_sets};
use crate::pass::analysis::cfg::ControlFlowGraph;
use crate::pass::analysis::rpo::RevresePostOrder;
use crate::pass::opt::lcm::earliest::EarliestExpression;
use crate::pass::{AnalysisPass, FormatTable};

pub fn postponable_expression_anaylsis(
    function: &Function,
    earliest: &EarliestExpression,
    cfg: &ControlFlowGraph,
    rpo: &RevresePostOrder,
) -> PostponableExpression {
    let mut pass = PostponableExpressionPass::new(earliest, cfg, rpo);
    pass.process(function)
}

pub struct PostponableExpression {
    postponable_expr_in: HashMap<Block, HashSet<InstOperandKey>>,
    postponable_expr_out: HashMap<Block, HashSet<InstOperandKey>>,
    downward_expose_expr: HashMap<Block, HashSet<InstOperandKey>>,
}

impl PostponableExpression {
    pub fn new() -> Self {
        Self {
            postponable_expr_in: Default::default(),
            postponable_expr_out: Default::default(),
            downward_expose_expr: Default::default(),
        }
    }
    /// Get postponable.in of given block.
    pub fn get_postponable_in(&self, block: Block) -> &HashSet<InstOperandKey> {
        self.postponable_expr_in.get(&block).unwrap()
    }
    /// Get downward exposed expression of given block.
    pub fn get_downward_exposed_expr(&self, block: Block) -> &HashSet<InstOperandKey> {
        self.downward_expose_expr.get(&block).unwrap()
    }
}

impl FormatTable for PostponableExpression {
    fn format_table(&self, func: &Function, _module: &Module) -> String {
        let mut format_string = String::new();
        for block in func.blocks() {
            format_string.push_str(&format!("Block{}\n", block.0));
            format_string.push_str(&format!("\tPostponable In:\n"));
            for key in self.postponable_expr_in.get(&block).unwrap() {
                format_string.push_str(&format!("\t\t{}\n", key.fmt_key()));
            }
            format_string.push_str(&format!("\tPostponable Out:\n"));
            for key in self.postponable_expr_out.get(&block).unwrap() {
                format_string.push_str(&format!("\t\t{}\n", key.fmt_key()));
            }
            format_string.push_str(&format!("\tDownward exposed expr:\n"));
            for key in self.downward_expose_expr.get(&block).unwrap() {
                format_string.push_str(&format!("\t\t{}\n", key.fmt_key()));
            }
        }
        format_string
    }
}

pub struct PostponableExpressionPass<'a> {
    earliest: &'a EarliestExpression,
    cfg: &'a ControlFlowGraph,
    rpo: &'a RevresePostOrder,
}

impl<'a> AnalysisPass<PostponableExpression> for PostponableExpressionPass<'a> {
    fn process(&mut self, func: &Function) -> PostponableExpression {
        let mut pass = PostponableExpression::new();
        self.run(func, &mut pass);
        pass
    }
}

impl<'a> PostponableExpressionPass<'a> {
    pub fn new(earliest: &'a EarliestExpression, cfg: &'a ControlFlowGraph, rpo: &'a RevresePostOrder) -> Self {
        Self { earliest, cfg, rpo }
    }
    fn run(&mut self, function: &Function, pass: &mut PostponableExpression) {
        self.init_sets(function, pass);
        self.compute_block_set(function, pass);
        self.iter_flow_equation(pass);
    }
    /// Init as forward data flow anaylsis
    fn init_sets(&mut self, function: &Function, pass: &mut PostponableExpression) {
        let keys = insts_to_keys(function.insts(), function);
        for block in function.blocks() {
            pass.postponable_expr_out.insert(block, keys.clone());
            pass.postponable_expr_in.insert(block, Default::default());
            pass.downward_expose_expr.insert(block, Default::default());
        }
        pass.postponable_expr_out
            .insert(self.cfg.get_entry(), Default::default());
    }
    /// Compute block independent set
    fn compute_block_set(&mut self, function: &Function, pass: &mut PostponableExpression) {
        for block in function.blocks() {
            let mut downward_exposed_expr = insts_to_keys(function.get_insts_of_block(block), function);
            for inst in function.get_insts_of_block(block) {
                if let Some(result) = function.get_inst_result(inst) {
                    downward_exposed_expr.retain(|key| !key.contain_operand(result));
                }
            }
            pass.downward_expose_expr.insert(block, downward_exposed_expr);
        }
    }
    /// Run data flow equation of postponable expression
    fn iter_flow_equation(&mut self, pass: &mut PostponableExpression) {
        let mut is_changed = true;
        let rpo = self.rpo.get_blocks_in_rpo();
        while is_changed {
            is_changed = false;
            for block in &rpo {
                let in_set = {
                    let predecessors = self.cfg.get_predecessors(block);
                    let out_sets_of_predecessors: Vec<&HashSet<InstOperandKey>> = predecessors
                        .iter()
                        .map(|predecessor| pass.postponable_expr_out.get(predecessor).unwrap())
                        .collect();
                    intersection_sets(out_sets_of_predecessors)
                };
                let next_out_set = {
                    let mut set = union_sets(vec![&in_set, self.earliest.get_earliest(*block)]);
                    set.retain(|key| !pass.downward_expose_expr.get(block).unwrap().contains(key));
                    set
                };
                let current_out_set = pass.postponable_expr_out.get(block).unwrap();
                pass.postponable_expr_in.insert(*block, in_set);
                if *current_out_set != next_out_set {
                    is_changed = true;
                    pass.postponable_expr_out.insert(*block, next_out_set);
                }
            }
        }
    }
}
