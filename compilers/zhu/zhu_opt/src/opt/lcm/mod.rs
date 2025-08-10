pub mod anticipate_expr;
pub mod critical_edge;
pub mod earliest;
pub mod later;
pub mod postponable_expr;
pub mod replacement;
pub mod used_expr;
pub mod will_be_available_expr;

use anticipate_expr::anticipate_expression_anaylsis;
use critical_edge::critical_edge_opt;
use earliest::earliest_expression_anaylsis;
use later::later_expression_anaylsis;
use postponable_expr::postponable_expression_anaylsis;
use replacement::replacement_opti_pass;
use used_expr::used_expression_anaylsis;
use will_be_available_expr::will_be_available_expression_anaylsis;

use crate::analysis::cfg::{cfg_anylysis, ControlFlowGraph};
use crate::analysis::rpo::{revrese_post_order_analysis, RevresePostOrder};
use crate::OptiPass;
use zhu_ir::entities::function::Function;

pub fn lcm_opt(cfg: &ControlFlowGraph, rpo: &RevresePostOrder, function: &mut Function) {
    let mut pass = LCMPass::new(cfg, rpo);
    pass.process(function);
}

pub struct LCMPass<'a> {
    cfg: &'a ControlFlowGraph,
    rpo: &'a RevresePostOrder,
}

impl<'a> OptiPass for LCMPass<'a> {
    fn process(&mut self, function: &mut Function) {
        critical_edge_opt(self.cfg, self.rpo, function);
        let cfg = cfg_anylysis(function);
        let rpo = revrese_post_order_analysis(&cfg);
        let anticipate_expr = anticipate_expression_anaylsis(&function, &cfg, &rpo);
        let will_be_available_expr = will_be_available_expression_anaylsis(function, &cfg, &rpo, &anticipate_expr);
        let earliest_expr = earliest_expression_anaylsis(function, &anticipate_expr, &will_be_available_expr);
        let postponeable_expr = postponable_expression_anaylsis(function, &earliest_expr, &cfg, &rpo);
        let later_expr = later_expression_anaylsis(function, &cfg, &earliest_expr, &postponeable_expr);
        let used_expr = used_expression_anaylsis(&function, &anticipate_expr, &cfg, &rpo, &later_expr);
        replacement_opti_pass(function, &cfg, &postponeable_expr, &later_expr, &used_expr);
    }
}

impl<'a> LCMPass<'a> {
    pub fn new(cfg: &'a ControlFlowGraph, rpo: &'a RevresePostOrder) -> Self {
        Self { cfg, rpo }
    }
}
