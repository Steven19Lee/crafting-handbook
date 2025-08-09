use std::collections::{HashMap, HashSet};

use crate::builder::FunctionBuilder;
use crate::entities::block::Block;
use crate::entities::function::Function;
use crate::entities::instruction::{insts_to_keys, opcode::OpCode, InstOperandKey, InstructionData};
use crate::entities::util::set_operation::{intersection_sets, union_sets};
use crate::entities::value::Value;
use crate::pass::analysis::cfg::ControlFlowGraph;
use crate::pass::opt::lcm::postponable_expr::PostponableExpression;
use crate::pass::OptiPass;

use crate::pass::opt::lcm::later::LaterExpression;
use crate::pass::opt::lcm::used_expr::UsedExpression;

#[derive(Debug)]
struct ReplacementContext {
    insert_blocks: HashSet<Block>,
    remove_blocks: HashSet<Block>,
}

pub fn replacement_opti_pass(
    function: &mut Function,
    cfg: &ControlFlowGraph,
    postponeable_expr: &PostponableExpression,
    later: &LaterExpression,
    used_expr: &UsedExpression,
) {
    let mut pass = ReplacementPass::new(cfg, postponeable_expr, later, used_expr);
    pass.process(function);
}

pub struct ReplacementPass<'a> {
    table: HashMap<InstOperandKey, ReplacementContext>,
    cfg: &'a ControlFlowGraph,
    postponeable_expr: &'a PostponableExpression,
    later: &'a LaterExpression,
    used_expr: &'a UsedExpression,
}

impl<'a> OptiPass for ReplacementPass<'a> {
    fn process(&mut self, func: &mut Function) {
        self.run(func);
    }
}

impl<'a> ReplacementPass<'a> {
    pub fn new(
        cfg: &'a ControlFlowGraph,
        postponeable_expr: &'a PostponableExpression,
        later: &'a LaterExpression,
        used_expr: &'a UsedExpression,
    ) -> Self {
        Self {
            table: Default::default(),
            cfg,
            postponeable_expr,
            later,
            used_expr,
        }
    }

    fn run(&mut self, function: &mut Function) {
        self.find_replacement_context(function);
        self.insert_insts_according_to_context(function);
        self.insert_phi_and_remove_partial_redundancy(function);
    }
    ///
    fn find_replacement_context(&mut self, function: &Function) {
        for block in function.blocks() {
            let keys_in_block = insts_to_keys(function.get_insts_of_block(block), function);
            // If expr in later(b) && used.out(b)
            // add it into insert context.
            // NOTE: if block already has instruction,
            // don't perform insert side effect.
            let insert_insts = intersection_sets(vec![
                self.later.get_later(block),
                self.used_expr.get_used_expr_out(block),
            ]);
            insert_insts
                .iter()
                .filter(|key| !keys_in_block.contains(key))
                .for_each(|key| {
                    self.add_insert_context(key, block);
                });
            // If expr in (down_use(b) & (!later(b) | used.out(b) ))
            // add it into remove context.
            // NOTE: if block also in insert context, do not
            // perform remove side effect
            let remove_insts = {
                let mut not_later_set = insts_to_keys(function.insts(), function);
                not_later_set.retain(|key| !self.later.get_later(block).contains(key));
                intersection_sets(vec![
                    self.postponeable_expr.get_downward_exposed_expr(block),
                    &union_sets(vec![&not_later_set, self.used_expr.get_used_expr_out(block)]),
                ])
            };
            remove_insts
                .iter()
                .filter(|key| !insert_insts.contains(&key))
                .for_each(|key| {
                    self.add_remove_context(key, block);
                });
        }
    }
    fn insert_insts_according_to_context(&mut self, function: &mut Function) {
        for (key, context) in &self.table {
            let inst_data: InstructionData = key.into();
            let ty = key.get_result_value_type(function);
            for target_block in &context.insert_blocks {
                let mut builder = FunctionBuilder::new(function);
                builder.switch_to_block(*target_block);
                let (_, inst) = builder.build_inst_and_result_entities(inst_data.clone(), ty.clone());
                function.layout.unshift_inst(inst, *target_block);
            }
        }
    }
    fn insert_phi_and_remove_partial_redundancy(&mut self, function: &mut Function) {
        for (key, context) in &self.table {
            for block in &context.remove_blocks {
                let mut phi_source: Vec<(Block, Value)> = Vec::new();
                for predecessor in self.cfg.get_predecessors(&block) {
                    self.dfs_find_phi_source(
                        *predecessor,
                        key,
                        &mut Default::default(),
                        function,
                        *predecessor,
                        &mut phi_source,
                    );
                }
                // build phi
                let mut builder = FunctionBuilder::new(function);
                builder.switch_to_block(*block);
                let phi_result = builder.phi_inst(phi_source);
                // remove inst if match key in block
                for inst in function.get_insts_of_block(*block) {
                    let inst_key: Option<InstOperandKey> = function.get_inst_data(inst).into();
                    if let Some(inst_key) = inst_key {
                        if *key == inst_key {
                            function.replace_inst(
                                inst,
                                InstructionData::Move {
                                    opcode: OpCode::Mov,
                                    src: phi_result,
                                },
                            );
                        }
                    }
                }
            }
        }
    }
}

impl<'a> ReplacementPass<'a> {
    fn add_remove_context(&mut self, key: &InstOperandKey, block: Block) {
        if let Some(entry) = self.table.get_mut(&key) {
            entry.remove_blocks.insert(block);
        } else {
            self.table.insert(
                key.clone(),
                ReplacementContext {
                    insert_blocks: Default::default(),
                    remove_blocks: HashSet::from([block]),
                },
            );
        }
    }
    fn add_insert_context(&mut self, key: &InstOperandKey, block: Block) {
        if let Some(entry) = self.table.get_mut(&key) {
            entry.insert_blocks.insert(block);
        } else {
            self.table.insert(
                key.clone(),
                ReplacementContext {
                    insert_blocks: HashSet::from([block]),
                    remove_blocks: Default::default(),
                },
            );
        }
    }
    fn dfs_find_phi_source(
        &self,
        current: Block,
        target_key: &InstOperandKey,
        visited: &mut HashSet<Block>,
        function: &Function,
        predecessor: Block,
        phi_souce: &mut Vec<(Block, Value)>,
    ) {
        if visited.contains(&current) {
            return;
        }
        visited.insert(current);
        let mut insts = function.get_insts_of_block(current);
        insts.reverse();
        for inst in insts {
            let inst_key: Option<InstOperandKey> = function.get_inst_data(inst).into();
            if let Some(key) = inst_key {
                if key == *target_key {
                    phi_souce.push((predecessor, function.get_inst_result(inst).unwrap()));
                    return;
                }
            }
        }
        for prede in self.cfg.get_predecessors(&current) {
            self.dfs_find_phi_source(*prede, target_key, visited, function, predecessor, phi_souce);
        }
    }
}
