use std::collections::{HashMap, HashSet};

use crate::entities::block::Block;
use crate::entities::function::Function;
use crate::entities::instruction::{insts_to_keys, InstOperandKey};
use crate::entities::module::Module;
use crate::entities::util::set_operation::{intersection_sets, union_sets};
use crate::entities::value::Value;
use crate::pass::analysis::cfg::ControlFlowGraph;
use crate::pass::analysis::rpo::RevresePostOrder;
use crate::pass::{get_table_header, AnalysisPass, FormatTable};

/// Create anticipate expression anaylsis result.
pub fn anticipate_expression_anaylsis(
    func: &Function,
    cfg: &ControlFlowGraph,
    rpo: &RevresePostOrder,
) -> AnticipateExpression {
    let mut pass = AnticipateExpressionPass::new(cfg, rpo);
    pass.process(func)
}
/// A expression is a anticipate in program point p if and only
/// if every path from p to exits will evaluate expression and
/// operand of expression is available at that time.
pub struct AnticipateExpression {
    anticipate_expr_in: HashMap<Block, HashSet<InstOperandKey>>,
    anticipate_expr_out: HashMap<Block, HashSet<InstOperandKey>>,
    kill: HashMap<Block, HashSet<Value>>,
    upward_exposed_expr: HashMap<Block, HashSet<InstOperandKey>>,
}

impl FormatTable for AnticipateExpression {
    fn format_table(&self, func: &Function, _module: &Module) -> String {
        let mut format_string = get_table_header("Anticipated Expression");
        for block in func.blocks() {
            format_string.push_str(&format!("Block{}:\n", block.0));
            format_string.push_str(&format!("\tAnticipate In:\n"));
            for key in self.anticipate_expr_in.get(&block).unwrap() {
                format_string.push_str(&format!("\t\t{}\n", key.fmt_key()));
            }
            format_string.push_str(&format!("\tAnticipate Out:\n"));
            for key in self.anticipate_expr_out.get(&block).unwrap() {
                format_string.push_str(&format!("\t\t{}\n", key.fmt_key()));
            }
            format_string.push_str(&format!("\tUpward exposed expr:\n"));
            for key in self.upward_exposed_expr.get(&block).unwrap() {
                format_string.push_str(&format!("\t\t{}\n", key.fmt_key()));
            }
            format_string.push_str(&format!("\tKill:\n"));
            for val in self.kill.get(&block).unwrap() {
                format_string.push_str(&format!("\t\treg{}\n", val.0));
            }
        }
        format_string
    }
}

impl AnticipateExpression {
    pub fn new() -> Self {
        Self {
            anticipate_expr_in: Default::default(),
            anticipate_expr_out: Default::default(),
            kill: Default::default(),
            upward_exposed_expr: Default::default(),
        }
    }
    /// Get anticipate expression in set of block
    /// panic if block is not find.
    pub fn get_anticipate_in(&self, block: Block) -> &HashSet<InstOperandKey> {
        self.anticipate_expr_in.get(&block).unwrap()
    }
    /// Get kill set of a block, panic if block
    /// is not find.
    pub fn get_kill_set(&self, block: Block) -> &HashSet<Value> {
        self.kill.get(&block).unwrap()
    }
    pub fn get_upward_exposed_expr(&self, block: Block) -> &HashSet<InstOperandKey> {
        self.upward_exposed_expr.get(&block).unwrap()
    }
}
pub struct AnticipateExpressionPass<'a> {
    cfg: &'a ControlFlowGraph,
    rpo: &'a RevresePostOrder,
}

impl<'a> AnalysisPass<AnticipateExpression> for AnticipateExpressionPass<'a> {
    fn process(&mut self, func: &Function) -> AnticipateExpression {
        let mut pass = AnticipateExpression::new();
        self.run(func, &mut pass);
        pass
    }
}

impl<'a> AnticipateExpressionPass<'a> {
    /// Create a new anticipate expression pass.
    pub fn new(cfg: &'a ControlFlowGraph, rpo: &'a RevresePostOrder) -> Self {
        Self { cfg, rpo }
    }
    fn run(&self, func: &Function, pass: &mut AnticipateExpression) {
        self.init_sets(func, pass);
        self.compute_block_set(func, pass);
        self.iter_flow_equation(pass);
    }
    /// Init in and out set as backward data analysis
    fn init_sets(&self, func: &Function, pass: &mut AnticipateExpression) {
        let insts = insts_to_keys(func.insts(), func);
        for block in func.blocks() {
            pass.anticipate_expr_in.insert(block, insts.clone());
            pass.anticipate_expr_out.insert(block, Default::default());
        }
        for exit in self.cfg.exists.clone() {
            pass.anticipate_expr_in.insert(exit, Default::default());
        }
    }
    /// Get kill and upward-exposed expression set of each block
    fn compute_block_set(&self, func: &Function, pass: &mut AnticipateExpression) {
        for block in func.blocks() {
            let mut insts_in_order = func.get_insts_of_block(block);
            insts_in_order.reverse();
            let mut kill: HashSet<Value> = Default::default();
            let mut upward_exposed_exprs: HashSet<InstOperandKey> = {
                let mut inst_keys: HashSet<InstOperandKey> = Default::default();
                for inst in &insts_in_order {
                    let inst_data = func.get_inst_data(*inst);
                    let inst_key: Option<InstOperandKey> = inst_data.into();
                    if let Some(k) = inst_key {
                        inst_keys.insert(k);
                    }
                }
                inst_keys
            };
            for inst in insts_in_order {
                if let Some(result_value) = func.get_inst_result(inst) {
                    kill.insert(result_value);
                    upward_exposed_exprs.retain(|expr| !expr.contain_operand(result_value));
                }
            }
            pass.kill.insert(block, kill);
            pass.upward_exposed_expr.insert(block, upward_exposed_exprs);
        }
    }
    /// Iterative backward data flow anaylsis algorithm for anticipate expression
    fn iter_flow_equation(&self, pass: &mut AnticipateExpression) {
        let mut is_changed = true;
        let mut po = self.rpo.get_blocks_in_rpo();
        po.reverse();
        while is_changed {
            is_changed = false;
            for block in &po {
                let out_set = {
                    let sucessors = self.cfg.get_successors(block);
                    let in_sets_of_sucessors: Vec<&HashSet<InstOperandKey>> = sucessors
                        .iter()
                        .map(|sucessor| pass.anticipate_expr_in.get(sucessor).unwrap())
                        .collect();
                    intersection_sets(in_sets_of_sucessors)
                };
                let next_in_set = {
                    let mut cloned_out_set = out_set.clone();
                    for kill_value in pass.kill.get(block).unwrap() {
                        cloned_out_set.retain(|inst| !inst.contain_operand(*kill_value));
                    }
                    union_sets(vec![&cloned_out_set, pass.upward_exposed_expr.get(block).unwrap()])
                };
                pass.anticipate_expr_out.insert(*block, out_set);
                let current_in_set = pass.anticipate_expr_in.get(block).unwrap();
                if *current_in_set != next_in_set {
                    pass.anticipate_expr_in.insert(*block, next_in_set);
                    is_changed = true;
                }
            }
        }
    }
}
