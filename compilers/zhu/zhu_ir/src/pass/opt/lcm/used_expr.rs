use std::collections::{HashMap, HashSet};

use crate::entities::block::Block;
use crate::entities::function::Function;
use crate::entities::instruction::InstOperandKey;
use crate::entities::module::Module;
use crate::entities::util::set_operation::union_sets;
use crate::pass::analysis::cfg::ControlFlowGraph;
use crate::pass::analysis::rpo::RevresePostOrder;
use crate::pass::opt::lcm::anticipate_expr::AnticipateExpression;
use crate::pass::opt::lcm::later::LaterExpression;
use crate::pass::{get_table_header, AnalysisPass, FormatTable};

pub fn used_expression_anaylsis(
    function: &Function,
    anticipate: &AnticipateExpression,
    cfg: &ControlFlowGraph,
    rpo: &RevresePostOrder,
    later: &LaterExpression,
) -> UsedExpression {
    let mut pass = UsedExpressionPass::new(anticipate, cfg, rpo, later);
    pass.process(function)
}

pub struct UsedExpression {
    used_expr_in: HashMap<Block, HashSet<InstOperandKey>>,
    used_expr_out: HashMap<Block, HashSet<InstOperandKey>>,
}

impl UsedExpression {
    pub fn new() -> Self {
        Self {
            used_expr_in: Default::default(),
            used_expr_out: Default::default(),
        }
    }
    /// Get Used.out(block) of given block,
    /// panic if block is not exit.
    pub fn get_used_expr_out(&self, block: Block) -> &HashSet<InstOperandKey> {
        self.used_expr_out.get(&block).unwrap()
    }
}

impl FormatTable for UsedExpression {
    fn format_table(&self, func: &Function, _module: &Module) -> String {
        let mut format_string = get_table_header("Used Expression");
        for block in func.blocks() {
            format_string.push_str(&format!("Block{}\n", block.0));
            format_string.push_str(&format!("\tUsed Expression In:\n"));
            for key in self.used_expr_in.get(&block).unwrap() {
                format_string.push_str(&format!("\t\t{}\n", key.fmt_key()));
            }
            format_string.push_str(&format!("\tUsed Expressiom Out:\n"));
            for key in self.used_expr_out.get(&block).unwrap() {
                format_string.push_str(&format!("\t\t{}\n", key.fmt_key()));
            }
        }
        format_string
    }
}

pub struct UsedExpressionPass<'a> {
    anticipate: &'a AnticipateExpression,
    cfg: &'a ControlFlowGraph,
    rpo: &'a RevresePostOrder,
    later: &'a LaterExpression,
}

impl<'a> AnalysisPass<UsedExpression> for UsedExpressionPass<'a> {
    fn process(&mut self, func: &Function) -> UsedExpression {
        let mut pass = UsedExpression::new();
        self.run(func, &mut pass);
        pass
    }
}

impl<'a> UsedExpressionPass<'a> {
    pub fn new(
        anticipate: &'a AnticipateExpression,
        cfg: &'a ControlFlowGraph,
        rpo: &'a RevresePostOrder,
        later: &'a LaterExpression,
    ) -> Self {
        Self {
            anticipate,
            cfg,
            rpo,
            later,
        }
    }
    fn run(&self, function: &Function, pass: &mut UsedExpression) {
        self.init_sets(function, pass);
        self.iter_flow_equation(pass);
    }
    fn init_sets(&self, function: &Function, pass: &mut UsedExpression) {
        for block in function.blocks() {
            pass.used_expr_in.insert(block, Default::default());
            pass.used_expr_out.insert(block, Default::default());
        }
    }
    fn iter_flow_equation(&self, pass: &mut UsedExpression) {
        let mut is_changed = true;
        let mut po = self.rpo.get_blocks_in_rpo();
        po.reverse();
        while is_changed {
            is_changed = false;
            for block in &po {
                let out_set = union_sets(
                    self.cfg
                        .get_successors(block)
                        .iter()
                        .map(|successor| pass.used_expr_in.get(successor).unwrap())
                        .collect(),
                );
                let mut next_in_set = union_sets(vec![&out_set, self.anticipate.get_upward_exposed_expr(*block)]);
                let later = self.later.get_later(*block);
                next_in_set.retain(|key| !later.contains(key));
                let current_in_set = pass.used_expr_in.get(block).unwrap();
                pass.used_expr_out.insert(*block, out_set);
                if *current_in_set != next_in_set {
                    is_changed = true;
                    pass.used_expr_in.insert(*block, next_in_set);
                }
            }
        }
    }
}
