use std::collections::HashSet;

use crate::analysis::cfg::ControlFlowGraph;
use crate::analysis::domtree::DomTree;
use crate::analysis::liveness::Liveness;
use crate::analysis::rpo::RevresePostOrder;
use crate::analysis::use_chain::UseTable;
use crate::OptiPass;
use zhu_ir::builder::FunctionBuilder;
use zhu_ir::entities::block::Block;
use zhu_ir::entities::function::Function;
use zhu_ir::entities::instruction::opcode::OpCode;
use zhu_ir::entities::instruction::Instruction;
use zhu_ir::entities::instruction::InstructionData;
use zhu_ir::entities::r#type::ValueType;
use zhu_ir::entities::value::Value;

pub fn mem2reg_pass<'a>(
    function: &mut Function,
    use_table: &'a UseTable,
    liveness: &'a Liveness,
    dom: &'a DomTree,
    cfg: &'a ControlFlowGraph,
    rpo: &'a RevresePostOrder,
) {
    let mut pass = Mem2RegPass {
        use_table,
        liveness,
        dom,
        cfg,
        rpo,
    };
    pass.process(function);
}

/// Context passing during renaming dfs traversal.
struct RenameContext {
    alloc_result: Value,
    phis_of_given_alloc: HashSet<Instruction>,
    phi_source_stack: Vec<(Block, Value)>,
}

pub struct Mem2RegPass<'a> {
    use_table: &'a UseTable,
    dom: &'a DomTree,
    cfg: &'a ControlFlowGraph,
    rpo: &'a RevresePostOrder,
    liveness: &'a Liveness,
}

impl OptiPass for Mem2RegPass<'_> {
    fn process(&mut self, func: &mut Function) {
        let promptable_alloc_insts = self.find_promptable_alloc_insts(func);
        for (alloc_inst, ty) in promptable_alloc_insts {
            if self.is_alloc_liveout(alloc_inst, &func) {
                let phi_of_given_alloc = self.insert_stage(func, alloc_inst, ty);
                self.renaming_stage(func, alloc_inst, &phi_of_given_alloc);
            } else {
                self.single_block_rename(alloc_inst, func);
            }
        }
    }
}

impl<'a> Mem2RegPass<'a> {
    /// Find alloca instructions that can be promoted to registers.
    /// - if this memory allocation is used as pointer, we can not promote it.
    ///   ( when the result of alloca is used as src in store instruction)
    /// - if thie memory allocation is load as different type, we can not promote it.
    fn find_promptable_alloc_insts(&self, function: &Function) -> HashSet<(Instruction, ValueType)> {
        let alloca_insts = function
            .insts()
            .into_iter()
            .filter(|&inst| matches!(*function.get_inst_data(inst), InstructionData::StackAlloc { .. }))
            .collect::<Vec<_>>();
        let mut promptable_alloc_insts: HashSet<(Instruction, ValueType)> = HashSet::new();
        for alloca in alloca_insts {
            let alloca_pointer = function.get_inst_result(alloca).unwrap();
            let uses = self.use_table.get_use(alloca_pointer).unwrap();
            let mut ty = None;
            for &use_inst in uses {
                let use_inst_data = function.get_inst_data(use_inst);
                match use_inst_data {
                    InstructionData::StoreRegister { src, .. } => {
                        if *src == alloca_pointer {
                            // the alloca is used as pointer, can not promote it.
                            break;
                        }
                    }
                    InstructionData::Call { params, .. } => {
                        if params.contains(&alloca_pointer) {
                            // the alloca is used as pointer, can not promote it.
                            break;
                        }
                    }
                    InstructionData::LoadRegister { .. } => {
                        let load_result = function.get_inst_result(use_inst).unwrap();
                        let load_ty = function.value_type(load_result).clone();
                        if let Some(t) = ty.clone() {
                            if t != load_ty {
                                break;
                            }
                        } else {
                            ty = Some(load_ty);
                        }
                    }
                    _ => {
                        // other instruction, ignore it.
                    }
                }
            }
            if let Some(ty) = ty {
                promptable_alloc_insts.insert((alloca, ty));
            }
        }
        promptable_alloc_insts
    }
    /// Insert phi instructions at the dominance frontier of blocks that contain store instructions
    /// which using the given alloca instruction.
    fn insert_stage(
        &self,
        function: &mut Function,
        promptable_alloc_inst: Instruction,
        value_type: ValueType,
    ) -> HashSet<Instruction> {
        // Get all store instructtion that use given alloc instruction.
        let alloc_result = function.get_inst_result(promptable_alloc_inst).unwrap();
        let store_insts = self
            .use_table
            .get_use(alloc_result)
            .unwrap()
            .into_iter()
            .filter(|&inst| matches!(*function.get_inst_data(*inst), InstructionData::StoreRegister { .. }))
            .collect::<Vec<_>>();
        // Get all blocks that contain store instruction. (dedpulicate)
        let blocks_of_store_insts = store_insts
            .iter()
            .map(|&inst| function.get_block_of_inst(*inst))
            .collect::<HashSet<_>>();
        let df_of_blocks_contain_store = blocks_of_store_insts
            .iter()
            .flat_map(|&block| self.dom.df(block))
            .collect::<HashSet<_>>();
        // Insert phi instruction at the dominance frontier of blocks that contain store instruction.
        let mut phi_insts = HashSet::new();
        for df_block in df_of_blocks_contain_store {
            let mut builder = FunctionBuilder::new(function);
            builder.switch_to_block(*df_block);
            let phi_inst = builder.phi_inst_with_empty_from(value_type.clone());
            phi_insts.insert(phi_inst);
        }
        phi_insts
    }
    fn renaming_stage(
        &self,
        function: &mut Function,
        alloc_inst: Instruction,
        phi_of_given_alloc: &HashSet<Instruction>,
    ) {
        let block = function.get_block_of_inst(alloc_inst);
        let mut context = RenameContext {
            alloc_result: function.get_inst_result(alloc_inst).unwrap(),
            phis_of_given_alloc: phi_of_given_alloc.clone(),
            phi_source_stack: vec![],
        };
        self.renaming_dfs_visit_block(function, block, &mut context);
    }

    fn renaming_dfs_visit_block(&self, function: &mut Function, block: Block, context: &mut RenameContext) {
        let mut stack_insert_count = 0;
        let mut insts_to_remove = HashSet::new();
        // Get all load and store instrution in current block that use the given alloc instruction.
        let mem_insts = function
            .get_insts_of_block(block)
            .into_iter()
            .filter(|&inst| {
                matches!(
                    *function.get_inst_data(inst),
                    InstructionData::StoreRegister { .. } | InstructionData::LoadRegister { .. }
                )
            })
            .filter(|&inst| self.use_table.get_use(context.alloc_result).unwrap().contains(&inst))
            .collect::<HashSet<_>>();
        // Rename load and store instruction.
        // - load inst: replace it with the value on the top of the stack.
        // - store inst: push the value to be stored onto the stack.
        for inst in mem_insts {
            let inst_data = function.get_inst_data(inst);
            match inst_data {
                InstructionData::LoadRegister { .. } => {
                    insts_to_remove.insert(inst);
                    if let Some((_, value)) = context.phi_source_stack.last() {
                        // Replace the load instruction with the value on the top of the stack.
                        let new_inst_data = InstructionData::Move {
                            opcode: OpCode::Mov,
                            src: *value,
                        };
                        function.replace_inst(inst, new_inst_data);
                    }
                }
                InstructionData::StoreRegister { src, .. } => {
                    insts_to_remove.insert(inst);
                    // The store instruction store to the given alloc instruction.
                    // Push the value to be stored onto the stack.
                    context.phi_source_stack.push((block, *src));
                    stack_insert_count += 1;
                }
                _ => {}
            }
        }
        // Update phi instructions in successor blocks.
        for successor in self.cfg.get_successors(&block) {
            let phi_insts = function
                .get_block_data(*successor)
                .phis
                .clone()
                .into_iter()
                .filter(|inst| context.phis_of_given_alloc.contains(&inst))
                .collect::<HashSet<_>>();
            for phi_inst in phi_insts {
                let phi_data = function.get_inst_data_mut(phi_inst);
                if let InstructionData::Phi { from, .. } = phi_data {
                    from.push(context.phi_source_stack.last().unwrap().clone());
                } else {
                    unreachable!("inst is not phi instruction");
                }
            }
        }
        // Remove load and store instruction.
        for inst in insts_to_remove {
            function.remove_inst(inst);
        }
        // Recursively visit child blocks in the dominator tree.
        for child in self
            .rpo
            .sort_blocks_in_rpo(self.dom.children(block).iter().map(|b| b.clone()).collect())
        {
            self.renaming_dfs_visit_block(function, child, context);
        }
        // Pop values from the stack that were pushed in this block.
        for _ in 0..stack_insert_count {
            context.phi_source_stack.pop();
        }
    }
    fn is_alloc_liveout(&self, alloc_inst: Instruction, function: &Function) -> bool {
        let block = function.get_block_of_inst(alloc_inst);
        let alloc_result = function.get_inst_result(alloc_inst).unwrap();
        self.liveness.get_live_out(block).contains(&alloc_result)
    }
    fn single_block_rename(&self, alloc_inst: Instruction, function: &mut Function) {
        let mut stack = Vec::new();
        let block = function.get_block_of_inst(alloc_inst);
        for inst in function.get_insts_of_block(block) {
            let inst_data = function.get_inst_data(inst);
            match inst_data {
                InstructionData::LoadRegister { .. } => {
                    if let Some((_, value)) = stack.last() {
                        // Replace the load instruction with the value on the top of the stack.
                        let new_inst_data = InstructionData::Move {
                            opcode: OpCode::Mov,
                            src: *value,
                        };
                        function.replace_inst(inst, new_inst_data);
                    }
                }
                InstructionData::StoreRegister { src, .. } => {
                    // The store instruction store to the given alloc instruction.
                    // Push the value to be stored onto the stack.
                    stack.push((block, *src));
                    function.remove_inst(inst);
                }
                _ => {}
            }
        }
    }
}
