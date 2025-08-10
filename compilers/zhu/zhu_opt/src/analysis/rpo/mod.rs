use crate::analysis::cfg::ControlFlowGraph;
use std::collections::{HashMap, HashSet};
use zhu_ir::entities::block::Block;

pub fn revrese_post_order_analysis(cfg: &ControlFlowGraph) -> RevresePostOrder {
    let mut rpo = RevresePostOrder::new();
    rpo.process(cfg);
    rpo
}

pub struct RevresePostOrder {
    block_map_rpo: HashMap<Block, usize>,
    blocks_in_rpo: Vec<Block>,
}
impl RevresePostOrder {
    pub fn new() -> Self {
        RevresePostOrder {
            block_map_rpo: HashMap::new(),
            blocks_in_rpo: Vec::new(),
        }
    }
    /// Process the control flow graph to compute the reverse post order
    pub fn process(&mut self, cfg: &ControlFlowGraph) {
        self.blocks_in_rpo.clear();
        self.block_map_rpo.clear();
        let mut visited = HashSet::new();
        let entry = cfg.get_entry();
        self.dfs_visit(entry, cfg, &mut visited);
        self.reverse();
    }
    /// Sort the blocks in reverse post order
    pub fn sort_blocks_in_rpo(&self, blocks: Vec<Block>) -> Vec<Block> {
        let mut sorted_blocks = blocks;
        sorted_blocks.sort_by_key(|block| self.block_map_rpo.get(block).unwrap());
        sorted_blocks
    }
    /// Get the blocks in reverse post order
    pub fn get_blocks_in_rpo(&self) -> Vec<Block> {
        self.blocks_in_rpo.clone()
    }
    pub fn get_block_rpo(&self, block: Block) -> usize {
        self.block_map_rpo.get(&block).unwrap().clone()
    }
    /// Post order visited blocks
    fn dfs_visit(&mut self, block: Block, cfg: &ControlFlowGraph, visited: &mut HashSet<Block>) {
        if visited.contains(&block) {
            return;
        }
        visited.insert(block.clone());
        for successor in cfg.get_successors(&block) {
            self.dfs_visit(successor.clone(), cfg, visited);
        }
        self.blocks_in_rpo.push(block.clone());
        self.block_map_rpo.insert(block, self.blocks_in_rpo.len());
    }
    /// Reverse the post order info
    fn reverse(&mut self) {
        self.blocks_in_rpo.reverse();
        let block_len = self.blocks_in_rpo.len();
        let block_map_rpo = std::mem::take(&mut self.block_map_rpo);
        for (block, po) in block_map_rpo {
            self.block_map_rpo.insert(block, block_len - po);
        }
    }
}
