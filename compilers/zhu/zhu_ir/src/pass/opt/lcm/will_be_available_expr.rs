use std::collections::{HashMap, HashSet};

use crate::entities::block::Block;
use crate::entities::function::Function;
use crate::entities::instruction::{insts_to_keys, InstOperandKey};
use crate::entities::module::Module;
use crate::entities::util::set_operation::{intersection_sets, union_sets};
use crate::pass::analysis::cfg::ControlFlowGraph;
use crate::pass::analysis::rpo::RevresePostOrder;
use crate::pass::opt::lcm::anticipate_expr::AnticipateExpression;
use crate::pass::{get_table_header, AnalysisPass, FormatTable};

pub fn will_be_available_expression_anaylsis(
    function: &Function,
    cfg: &ControlFlowGraph,
    rpo: &RevresePostOrder,
    anticipate: &AnticipateExpression,
) -> WillBeAvailableExpression {
    let mut pass = WillBeAvailableExpressionPass::new(cfg, rpo, anticipate);
    pass.process(function)
}

pub struct WillBeAvailableExpression {
    will_be_avail_expr_in: HashMap<Block, HashSet<InstOperandKey>>,
    will_be_avail_expr_out: HashMap<Block, HashSet<InstOperandKey>>,
}

impl WillBeAvailableExpression {
    pub fn new() -> Self {
        Self {
            will_be_avail_expr_in: Default::default(),
            will_be_avail_expr_out: Default::default(),
        }
    }
    pub fn get_will_be_available_in(&self, block: &Block) -> &HashSet<InstOperandKey> {
        self.will_be_avail_expr_in.get(block).unwrap()
    }
}

impl FormatTable for WillBeAvailableExpression {
    fn format_table(&self, func: &Function, _module: &Module) -> String {
        let mut format_string = get_table_header("Will be Available");
        for block in func.blocks() {
            format_string.push_str(&format!("Block{}:\n", block.0));
            format_string.push_str(&format!("\tWill be Available In:\n"));
            for key in self.will_be_avail_expr_in.get(&block).unwrap() {
                format_string.push_str(&format!("\t\t{}\n", key.fmt_key()));
            }
            format_string.push_str(&format!("\tWill be Available Out:\n"));
            for key in self.will_be_avail_expr_out.get(&block).unwrap() {
                format_string.push_str(&format!("\t\t{}\n", key.fmt_key()));
            }
        }
        format_string
    }
}

pub struct WillBeAvailableExpressionPass<'a> {
    cfg: &'a ControlFlowGraph,
    rpo: &'a RevresePostOrder,
    anticipate: &'a AnticipateExpression,
}

impl<'a> AnalysisPass<WillBeAvailableExpression> for WillBeAvailableExpressionPass<'a> {
    fn process(&mut self, func: &Function) -> WillBeAvailableExpression {
        let mut pass = WillBeAvailableExpression::new();
        self.run(func, &mut pass);
        pass
    }
}

impl<'a> WillBeAvailableExpressionPass<'a> {
    pub fn new(cfg: &'a ControlFlowGraph, rpo: &'a RevresePostOrder, anticipate: &'a AnticipateExpression) -> Self {
        Self { cfg, rpo, anticipate }
    }
    fn run(&self, func: &Function, pass: &mut WillBeAvailableExpression) {
        self.init_sets(func, pass);
        self.iter_flow_equation(pass);
    }
    fn init_sets(&self, function: &Function, pass: &mut WillBeAvailableExpression) {
        let insts: HashSet<InstOperandKey> = insts_to_keys(function.insts(), function);
        for block in function.blocks() {
            pass.will_be_avail_expr_in.insert(block, Default::default());
            pass.will_be_avail_expr_out.insert(block, insts.clone());
        }
        pass.will_be_avail_expr_out
            .insert(self.cfg.get_entry(), Default::default());
    }
    fn iter_flow_equation(&self, pass: &mut WillBeAvailableExpression) {
        let mut is_changed = true;
        let rpo = self.rpo.get_blocks_in_rpo();
        while is_changed {
            is_changed = false;
            for block in &rpo {
                // Compute in and out set
                let in_set = {
                    let predecessors = self.cfg.get_predecessors(block);
                    let out_sets_of_predecessors: Vec<&HashSet<InstOperandKey>> = predecessors
                        .iter()
                        .map(|predecessor| pass.will_be_avail_expr_out.get(predecessor).unwrap())
                        .collect();
                    intersection_sets(out_sets_of_predecessors)
                };
                let next_out_set = {
                    let mut set = union_sets(vec![&in_set, self.anticipate.get_anticipate_in(*block)]);
                    let kill_set = self.anticipate.get_kill_set(*block);
                    kill_set.iter().for_each(|value| {
                        set.retain(|inst| !inst.contain_operand(*value));
                    });
                    set
                };
                let current_out_set = pass.will_be_avail_expr_out.get(block).unwrap();
                // Insert in and out set
                pass.will_be_avail_expr_in.insert(*block, in_set);
                if *current_out_set != next_out_set {
                    is_changed = true;
                    pass.will_be_avail_expr_out.insert(*block, next_out_set);
                }
            }
        }
    }
}
