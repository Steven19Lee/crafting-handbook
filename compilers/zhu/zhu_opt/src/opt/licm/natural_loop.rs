use crate::analysis::cfg::ControlFlowGraph;
use crate::analysis::domtree::DomTree;
use std::collections::HashSet;
use zhu_ir::entities::block::Block;

pub fn natural_loop_analysis<'a>(dom: &'a DomTree, cfg: &'a ControlFlowGraph) -> Vec<NaturalLoop> {
    NaturalLoopAnalysis::new(dom, cfg).process()
}

#[derive(Debug, Clone)]
pub struct NaturalLoopAnalysis<'a> {
    pub dom: &'a DomTree,
    pub cfg: &'a ControlFlowGraph,
}
#[derive(Debug, Clone)]
pub struct NaturalLoop {
    pub header: Block,
    pub tail: Block,
    pub blocks: HashSet<Block>,
    pub exits: HashSet<Block>,
}

type Edge = (Block, Block);

impl<'a> NaturalLoopAnalysis<'a> {
    pub fn new(dom: &'a DomTree, cfg: &'a ControlFlowGraph) -> Self {
        Self { cfg, dom }
    }
    /// Here we use dominator to distinguish backward edge and cross edge.
    fn find_backward_edges_by_dfs(&self, vertex: Block, visited: &mut HashSet<Block>, edges: &mut Vec<Edge>) {
        visited.insert(vertex);
        for sucessor in self.cfg.get_successors(&vertex) {
            if visited.contains(sucessor) && self.dom.dominate(sucessor.clone(), vertex) {
                edges.push((vertex.clone(), sucessor.clone()));
            } else {
                self.find_backward_edges_by_dfs(sucessor.clone(), visited, edges);
            }
        }
    }
    /// Find backward edges in the control flow graph.
    fn find_backward_edges(&self) -> Vec<Edge> {
        let mut edges = Vec::new();
        self.find_backward_edges_by_dfs(self.cfg.get_entry(), &mut HashSet::new(), &mut edges);
        edges
    }
    /// find natural loop blocks by reverse DFS.
    fn find_natural_loop_blocks(&self, header: Block, vertex: Block, blocks: &mut HashSet<Block>) {
        if vertex == header {
            return;
        }
        blocks.insert(vertex.clone());
        for predecessor in self.cfg.get_predecessors(&vertex) {
            if !blocks.contains(predecessor) {
                self.find_natural_loop_blocks(header.clone(), predecessor.clone(), blocks);
            }
        }
    }
    /// Find exits of the natural loop.
    fn find_exits_block_for_natural_loop(&self, header: &Block, blocks: &HashSet<Block>, exits: &mut HashSet<Block>) {
        for block in blocks.iter() {
            if block == header {
                continue;
            }
            for successor in self.cfg.get_successors(block) {
                if !blocks.contains(successor) && !exits.contains(block) {
                    exits.insert(block.clone());
                    break;
                }
            }
        }
    }
    fn find_natural_loops(&self) -> Vec<NaturalLoop> {
        let backward_edges = self.find_backward_edges();
        let mut natural_loops: Vec<NaturalLoop> = Vec::new();
        for (tail, header) in backward_edges {
            let mut blocks = HashSet::from([header, tail]);
            self.find_natural_loop_blocks(header, tail, &mut blocks);
            let mut exits = HashSet::new();
            self.find_exits_block_for_natural_loop(&header, &blocks, &mut exits);
            natural_loops.push(NaturalLoop {
                header,
                tail,
                blocks,
                exits,
            });
        }
        natural_loops
    }
    pub fn process(&self) -> Vec<NaturalLoop> {
        self.find_natural_loops()
    }
}
