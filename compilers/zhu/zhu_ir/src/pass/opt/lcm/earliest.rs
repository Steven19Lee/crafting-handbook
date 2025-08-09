use std::collections::{HashMap, HashSet};

use crate::entities::block::Block;
use crate::entities::function::Function;
use crate::entities::instruction::InstOperandKey;
use crate::entities::module::Module;
use crate::pass::opt::lcm::anticipate_expr::AnticipateExpression;
use crate::pass::opt::lcm::will_be_available_expr::WillBeAvailableExpression;
use crate::pass::{get_table_header, AnalysisPass, FormatTable};

pub fn earliest_expression_anaylsis(
    function: &Function,
    anticipate: &AnticipateExpression,
    will_be_available: &WillBeAvailableExpression,
) -> EarliestExpression {
    let mut pass = EarliestExpressionPass::new(anticipate, will_be_available);
    pass.process(function)
}

pub struct EarliestExpression {
    earliest: HashMap<Block, HashSet<InstOperandKey>>,
}

impl EarliestExpression {
    pub fn new() -> Self {
        Self {
            earliest: Default::default(),
        }
    }
    pub fn get_earliest(&self, block: Block) -> &HashSet<InstOperandKey> {
        self.earliest.get(&block).unwrap()
    }
}

impl FormatTable for EarliestExpression {
    fn format_table(&self, func: &Function, _module: &Module) -> String {
        let mut format_string = get_table_header("Earliest");
        for block in func.blocks() {
            format_string.push_str(&format!("Block{}:\n", block.0));
            for key in self.get_earliest(block) {
                format_string.push_str(&format!("\t{}\n", key.fmt_key()))
            }
        }
        format_string
    }
}

pub struct EarliestExpressionPass<'a> {
    anticipate: &'a AnticipateExpression,
    will_be_available: &'a WillBeAvailableExpression,
}

impl<'a> AnalysisPass<EarliestExpression> for EarliestExpressionPass<'a> {
    fn process(&mut self, function: &Function) -> EarliestExpression {
        let mut pass = EarliestExpression::new();
        self.run(function, &mut pass);
        pass
    }
}

impl<'a> EarliestExpressionPass<'a> {
    pub fn new(anticipate: &'a AnticipateExpression, will_be_available: &'a WillBeAvailableExpression) -> Self {
        Self {
            anticipate,
            will_be_available,
        }
    }
    fn run(&self, function: &Function, pass: &mut EarliestExpression) {
        for block in function.blocks() {
            let mut set = self.anticipate.get_anticipate_in(block).clone();
            let will_be_available_in = self.will_be_available.get_will_be_available_in(&block);
            will_be_available_in.iter().for_each(|key| {
                set.remove(key);
            });
            pass.earliest.insert(block, set);
        }
    }
}
