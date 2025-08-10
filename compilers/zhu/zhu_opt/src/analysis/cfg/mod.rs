use std::collections::{HashMap, HashSet};
use zhu_ir::entities::block::Block;
use zhu_ir::entities::function::Function;
use zhu_ir::entities::instruction::InstructionData;

pub fn cfg_anylysis(function: &Function) -> ControlFlowGraph {
    let mut cfg = ControlFlowGraph::new();
    cfg.process(function);
    cfg
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct CFGNode {
    pub predecessors: HashSet<Block>,
    pub successors: HashSet<Block>,
}
#[derive(Debug, PartialEq, Clone, Eq)]
pub struct ControlFlowGraph {
    pub entry: Option<Block>,
    pub exists: HashSet<Block>,
    pub blocks: HashMap<Block, CFGNode>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            entry: None,
            exists: Default::default(),
            blocks: Default::default(),
        }
    }
    pub fn process(&mut self, function: &Function) {
        // init
        self.init(function);
        // compute loop
        for block in function.entities.blocks.keys() {
            let last_inst = function.layout.blocks.get(block).unwrap().last_inst.unwrap();
            let last_inst_data = function.entities.insts.get(&last_inst).unwrap();
            match last_inst_data {
                InstructionData::Jump { dst, .. } => {
                    self.connect(block, dst);
                }
                InstructionData::BrIf { conseq, alter, .. } => {
                    self.connect(block, conseq);
                    self.connect(block, alter);
                }
                InstructionData::Ret { .. } => {
                    self.exists.insert(block.clone());
                }
                _ => { /* Should be unreach */ }
            }
        }
    }
}

impl ControlFlowGraph {
    pub fn get_entry(&self) -> Block {
        self.entry.unwrap().clone()
    }
    pub fn get_exists(&self) -> &HashSet<Block> {
        &self.exists
    }
    pub fn get_predecessors(&self, block: &Block) -> &HashSet<Block> {
        &self.get_block_cfg_node(block).predecessors
    }
    pub fn get_successors(&self, block: &Block) -> &HashSet<Block> {
        &self.get_block_cfg_node(block).successors
    }
}

impl ControlFlowGraph {
    fn get_block_cfg_node(&self, block: &Block) -> &CFGNode {
        self.blocks
            .get(block)
            .expect(&format!("block {:?} not found in CFG", block))
    }
    fn connect(&mut self, predecessor: &Block, successor: &Block) {
        self.blocks
            .get_mut(predecessor)
            .unwrap()
            .successors
            .insert(successor.clone());
        self.blocks
            .get_mut(successor)
            .unwrap()
            .predecessors
            .insert(predecessor.clone());
    }
    fn init(&mut self, function: &Function) {
        for block in function.blocks() {
            self.blocks.insert(
                block,
                CFGNode {
                    predecessors: Default::default(),
                    successors: Default::default(),
                },
            );
        }
        self.entry = function.layout.first_block()
    }
}
