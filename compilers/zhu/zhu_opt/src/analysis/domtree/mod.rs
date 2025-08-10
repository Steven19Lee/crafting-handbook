use crate::analysis::cfg::ControlFlowGraph;
use crate::AnalysisPass;
use std::collections::{HashMap, HashSet};
use zhu_ir::entities::block::Block;
use zhu_ir::entities::function::Function;

pub fn domtree_analysis(func: &Function, cfg: &ControlFlowGraph) -> DomTree {
    let mut pass = DomTreePass::new(cfg);
    pass.process(func)
}
#[derive(Debug, Clone, PartialEq)]
struct DomTableEntry {
    immediate_dominator: Option<Block>,
    dominate_children: HashSet<Block>,
    dominators: HashSet<Block>,
    dominate_frontier: HashSet<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DomTree {
    table: HashMap<Block, DomTableEntry>,
}

impl DomTree {
    pub fn new() -> Self {
        Self {
            table: Default::default(),
        }
    }
    /// Is bloock a dominate block b
    /// -> Dom(b) contain a
    pub fn dominate(&self, a: Block, b: Block) -> bool {
        if let Some(entry) = self.table.get(&b) {
            entry.dominators.contains(&a)
        } else {
            panic!()
        }
    }
    /// Get dominators of block
    /// -> Dom(b)
    pub fn dom(&self, block: Block) -> &HashSet<Block> {
        if let Some(entry) = self.table.get(&block) {
            &entry.dominators
        } else {
            panic!()
        }
    }
    /// Get immediate dominator of block
    /// -> idom(b)
    pub fn idom(&self, block: Block) -> Option<Block> {
        if let Some(entry) = self.table.get(&block) {
            entry.immediate_dominator.clone()
        } else {
            panic!()
        }
    }
    /// Get dominate frontier of block
    /// -> DF(b)
    pub fn df(&self, block: Block) -> &HashSet<Block> {
        if let Some(entry) = self.table.get(&block) {
            &entry.dominate_frontier
        } else {
            panic!()
        }
    }
    pub fn children(&self, block: Block) -> &HashSet<Block> {
        if let Some(entry) = self.table.get(&block) {
            &entry.dominate_children
        } else {
            panic!()
        }
    }
}

pub struct DomTreePass<'a> {
    cfg: &'a ControlFlowGraph,
}

impl<'a> AnalysisPass<DomTree> for DomTreePass<'a> {
    fn process(&mut self, _func: &Function) -> DomTree {
        let mut dom_tree = DomTree::new();
        self.compute_dom(&mut dom_tree);
        self.compute_idom(&mut dom_tree);
        self.compute_df(&mut dom_tree);
        dom_tree
    }
}

impl<'a> DomTreePass<'a> {
    pub fn new(cfg: &'a ControlFlowGraph) -> Self {
        Self { cfg }
    }
    /// Compute dom by flow equation.
    ///
    /// Dominator is a forward flow anaylsis
    fn compute_dom(&mut self, dom_tree: &mut DomTree) {
        // init
        for block in self.cfg.blocks.keys() {
            let all_block: HashSet<Block> = self.cfg.blocks.keys().map(|b| b.clone()).collect();
            dom_tree.table.insert(
                block.clone(),
                DomTableEntry {
                    immediate_dominator: None,
                    dominate_children: Default::default(),
                    dominators: all_block,
                    dominate_frontier: Default::default(),
                },
            );
        }
        dom_tree.table.get_mut(&self.cfg.get_entry()).unwrap().dominators = HashSet::from([self.cfg.get_entry()]);
        // WorkList Iteration
        let mut is_change = true;
        while is_change {
            is_change = false;
            for block in self.cfg.blocks.keys() {
                let mut next_dom_of_bb = dom_tree.table.get_mut(block).unwrap().dominators.clone();
                for predecessor in self.cfg.get_predecessors(block) {
                    let dom_of_predecessor = &dom_tree.table.get_mut(&predecessor).unwrap().dominators;
                    next_dom_of_bb = next_dom_of_bb
                        .intersection(dom_of_predecessor)
                        .map(|b| b.clone())
                        .collect();
                }
                next_dom_of_bb.insert(block.clone());
                if next_dom_of_bb != dom_tree.table.get(block).unwrap().dominators {
                    dom_tree.table.get_mut(block).unwrap().dominators = next_dom_of_bb;
                    is_change = true;
                }
            }
        }
    }
    /// Compute immediate dominator
    ///
    /// Using BFS on predecessor edge to get the closest dominator
    fn compute_idom(&mut self, dom_tree: &mut DomTree) {
        for block in self.cfg.blocks.keys() {
            let mut worklist: Vec<Block> = self.cfg.get_predecessors(block).iter().map(|b| b.clone()).collect();
            let mut marks: HashSet<Block> = HashSet::from([block.clone()]);
            'backward_traversal: while worklist.len() != 0 {
                let mut next_worklist = Vec::new();
                for b in worklist.iter() {
                    if marks.contains(b) {
                        continue;
                    } else {
                        marks.insert(b.clone());
                        if dom_tree.table.get(block).unwrap().dominators.contains(b) {
                            dom_tree.table.get_mut(block).unwrap().immediate_dominator = Some(b.clone());
                            dom_tree
                                .table
                                .get_mut(b)
                                .unwrap()
                                .dominate_children
                                .insert(block.clone());
                            break 'backward_traversal;
                        }
                        next_worklist.extend(self.cfg.get_predecessors(b).iter().map(|b| b.clone()));
                    }
                }
                worklist = next_worklist;
            }
        }
    }
    fn compute_df(&mut self, dom_tree: &mut DomTree) {
        // collect join nodes
        let mut join_nodes = Vec::new();
        for (block, block_node) in &self.cfg.blocks {
            if block_node.predecessors.len() > 1 {
                join_nodes.push(block.clone());
            }
        }
        // for each join_node, it will be df of ancestor util idom
        for join_node in join_nodes {
            // when idom is none, node must be entry point and entry point can not
            // be DF of any node.
            let idom = if let Some(bb) = dom_tree.idom(join_node) {
                bb
            } else {
                continue;
            };
            for predeceesor_block in self.cfg.get_predecessors(&join_node) {
                let mut runner_id = predeceesor_block.clone();
                while runner_id != idom {
                    dom_tree
                        .table
                        .get_mut(&runner_id)
                        .unwrap()
                        .dominate_frontier
                        .insert(join_node);
                    runner_id = dom_tree.idom(runner_id).unwrap();
                }
            }
        }
    }
}
