use std::collections::HashSet;

use crate::analysis::cfg::{CFGNode, ControlFlowGraph};
use crate::analysis::domtree::domtree_analysis;
use crate::analysis::domtree::DomTree;
use crate::AnalysisPass;
use zhu_ir::entities::block::Block;
use zhu_ir::entities::function::Function;

pub fn post_domtree_analysis(func: &Function, cfg: &ControlFlowGraph) -> PostDomTree {
    let mut post_dommtree_pass = PostDomTreePass::new(cfg);
    post_dommtree_pass.process(func)
}

#[derive(Debug, Clone, PartialEq)]
pub struct PostDomTree {
    pub dom_tree: DomTree,
    pub exit: Option<Block>,
}

impl PostDomTree {
    /// Is post dominator need a extra block for single exit ?
    pub fn is_add_exit_block(&self) -> bool {
        self.exit.is_some()
    }
    /// Is bloock a post dominate block b
    /// -> Post_Dom(b) contain a
    pub fn post_dominate(&self, a: Block, b: Block) -> bool {
        self.dom_tree.dominate(a, b)
    }
    /// Get post dominators of block
    /// -> Post_Dom(b)
    pub fn post_dom(&self, block: Block) -> &HashSet<Block> {
        self.dom_tree.dom(block)
    }
    /// Get immediate post dominator of block
    /// -> post_idom(b)
    pub fn post_idom(&self, block: Block) -> Option<Block> {
        self.dom_tree.idom(block)
    }
    /// Get post dominate frontier of block
    /// -> Post_DF(b)
    pub fn post_df(&self, block: Block) -> &HashSet<Block> {
        self.dom_tree.df(block)
    }
    /// Get post childrens of blocks
    pub fn children(&self, block: Block) -> &HashSet<Block> {
        self.dom_tree.children(block)
    }
}

struct PostDomTreePass<'a> {
    cfg: &'a ControlFlowGraph,
}

impl<'a> AnalysisPass<PostDomTree> for PostDomTreePass<'a> {
    fn process(&mut self, func: &Function) -> PostDomTree {
        let (reverse_cfg, exit) = self.create_reverse_cfg();
        let dom_tree = domtree_analysis(func, &reverse_cfg);
        PostDomTree { dom_tree, exit }
    }
}

impl<'a> PostDomTreePass<'a> {
    pub fn new(cfg: &'a ControlFlowGraph) -> Self {
        Self { cfg }
    }
    fn create_reverse_cfg(&self) -> (ControlFlowGraph, Option<Block>) {
        let mut reverse_cfg = self.cfg.clone();
        // swap predeceesor and successor of every blocks
        for (_, cfg_node) in &mut reverse_cfg.blocks {
            std::mem::swap(&mut cfg_node.predecessors, &mut cfg_node.successors);
        }
        // If we only have one exit, just swap exit and entry
        if reverse_cfg.exists.len() == 1 {
            let original_exit = {
                let mut exit = None;
                for e in &reverse_cfg.exists {
                    if exit.is_none() {
                        exit = Some(e.clone());
                        break;
                    }
                }
                exit
            };
            let original_entry = self.cfg.get_entry();
            reverse_cfg.entry = original_exit;
            reverse_cfg.exists = HashSet::from([original_entry]);
            (reverse_cfg, None)
        // otherwise, create a new exit, connect original exits to exit
        // and swap entry and exit.
        } else {
            // find next block index
            let exit_block = {
                let mut max_block_index = 0;
                for b in self.cfg.blocks.keys() {
                    if b.0 > max_block_index {
                        max_block_index = b.0
                    }
                }
                Block(max_block_index + 1)
            };
            // Create new exit, connect to original exit as predecessors
            reverse_cfg.blocks.insert(
                exit_block,
                CFGNode {
                    predecessors: reverse_cfg.exists.clone(),
                    successors: Default::default(),
                },
            );
            for origin_exit in reverse_cfg.exists {
                let origin_exit_cfg_node = reverse_cfg.blocks.get_mut(&origin_exit).unwrap();
                origin_exit_cfg_node.predecessors = HashSet::from([exit_block]);
            }
            // swap exit and entry
            let original_entry = self.cfg.get_entry();
            reverse_cfg.entry = Some(exit_block);
            reverse_cfg.exists = HashSet::from([original_entry]);
            (reverse_cfg, Some(exit_block))
        }
    }
}
