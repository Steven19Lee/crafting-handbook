use crate::analysis::cfg::ControlFlowGraph;
use crate::analysis::rpo::RevresePostOrder;
use crate::AnalysisPass;
use std::collections::{HashMap, HashSet};

use zhu_ir::entities::block::Block;
use zhu_ir::entities::function::Function;
use zhu_ir::entities::instruction::InstructionCommon;
use zhu_ir::entities::util::set_operation::union_sets;
use zhu_ir::entities::value::Value;

pub fn liveness_analysis(func: &Function, cfg: &ControlFlowGraph, rpo: &RevresePostOrder) -> Liveness {
    let mut pass = LivenessPass::new(cfg, rpo);
    pass.process(func)
}

pub struct Liveness {
    live_in: HashMap<Block, HashSet<Value>>,
    live_out: HashMap<Block, HashSet<Value>>,
    def_set: HashMap<Block, HashSet<Value>>,
    // upward exposed uses
    use_set: HashMap<Block, HashSet<Value>>,
}

impl Liveness {
    pub fn new() -> Self {
        Self {
            live_in: Default::default(),
            live_out: Default::default(),
            def_set: Default::default(),
            use_set: Default::default(),
        }
    }
    /// Get liveOut set of given block
    pub fn get_live_out(&self, block: Block) -> &HashSet<Value> {
        self.live_out.get(&block).unwrap()
    }
}

pub struct LivenessPass<'a> {
    rpo: &'a RevresePostOrder,
    cfg: &'a ControlFlowGraph,
}

impl<'a> AnalysisPass<Liveness> for LivenessPass<'a> {
    fn process(&mut self, func: &Function) -> Liveness {
        let mut pass = Liveness::new();
        self.init_sets(func, &mut pass);
        self.compute_single_block_sets(func, &mut pass);
        self.iter_flow_equation(&mut pass);
        pass
    }
}

impl<'a> LivenessPass<'a> {
    /// Create a new liveness pass.
    pub fn new(cfg: &'a ControlFlowGraph, rpo: &'a RevresePostOrder) -> Self {
        Self { cfg, rpo }
    }
    fn init_sets(&self, function: &Function, liveness: &mut Liveness) {
        for block in function.blocks() {
            liveness.def_set.insert(block, HashSet::new());
            liveness.use_set.insert(block, HashSet::new());
            liveness.live_in.insert(block, HashSet::new());
            liveness.live_out.insert(block, HashSet::new());
        }
    }
    fn compute_single_block_sets(&self, function: &Function, liveness: &mut Liveness) {
        for block in function.blocks() {
            let mut def_set = HashSet::new();
            let mut use_set = HashSet::new();
            for inst in function.get_insts_of_block(block) {
                if let Some(dest) = function.get_inst_result(inst) {
                    def_set.insert(dest);
                }
                let inst_data = function.get_inst_data(inst);
                for operand in inst_data.get_operands() {
                    if def_set.contains(&operand) {
                        continue;
                    }
                    use_set.insert(operand);
                }
            }
            liveness.def_set.insert(block, def_set);
            liveness.use_set.insert(block, use_set);
        }
    }
    fn iter_flow_equation(&self, liveness: &mut Liveness) {
        let mut is_changed = true;
        let mut po = self.rpo.get_blocks_in_rpo();
        po.reverse();
        while is_changed {
            is_changed = false;
            for block in &po {
                let mut out_set = {
                    let successors = self.cfg.get_successors(&block);
                    let in_sets_of_sucessors: Vec<&HashSet<Value>> = successors
                        .iter()
                        .map(|succ| liveness.live_in.get(succ).unwrap())
                        .collect();
                    union_sets(in_sets_of_sucessors)
                };
                liveness.live_out.insert(*block, out_set.clone());
                let next_in_set = {
                    // out[b] - def[b]
                    out_set.retain(|value| liveness.def_set.get(block).unwrap().get(value).is_none());
                    union_sets(vec![&out_set, liveness.use_set.get(block).unwrap()])
                };
                let current_in_set = liveness.live_in.get(block).unwrap();
                if next_in_set != *current_in_set {
                    liveness.live_in.insert(*block, next_in_set);
                    is_changed = true;
                }
            }
        }
    }
}
