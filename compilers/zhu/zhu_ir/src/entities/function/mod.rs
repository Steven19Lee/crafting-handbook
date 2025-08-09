use crate::entities::block::{Block, BlockData};
use crate::entities::constant::{Constant, ConstantData};
use crate::entities::external_name::ExternalName;
use crate::entities::function::entites::FunctionEntities;
use crate::entities::function::layout::FunctionLayout;
use crate::entities::global_value::{GlobalValue, GlobalValueData};
use crate::entities::instruction::{Instruction, InstructionData};
use crate::entities::r#type::{MemType, MemTypeData, ValueType};
use crate::entities::value::{Value, ValueData};
use std::collections::HashMap;

pub mod entites;
pub mod layout;

#[derive(Debug, PartialEq, Clone, Eq, Hash, Copy)]
pub struct FunctionRef(pub u32);
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionSignature {
    pub params: Vec<ValueType>,
    pub return_type: Option<ValueType>,
}

impl FunctionSignature {
    pub fn new() -> Self {
        Self {
            params: Default::default(),
            return_type: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExternalFunctionData {
    pub sig: FunctionSignature,
    pub name: ExternalName,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    // signature
    pub signature: FunctionSignature,
    // function body
    pub entities: FunctionEntities,
    pub layout: FunctionLayout,
    // function info
    pub mem_type: HashMap<MemType, MemTypeData>,
    pub constants: HashMap<Constant, ConstantData>,
    // (might be) external info
    pub external_funcs: HashMap<FunctionRef, ExternalFunctionData>,
    pub global_values: HashMap<GlobalValue, GlobalValueData>,
}

impl Function {
    pub fn new() -> Self {
        Self {
            signature: FunctionSignature::new(),
            entities: FunctionEntities::new(),
            layout: FunctionLayout::new(),
            mem_type: Default::default(),
            constants: Default::default(),
            external_funcs: Default::default(),
            global_values: Default::default(),
        }
    }
}
/// Data mutation to signature.
impl Function {
    pub fn def_func_param(&mut self, ty: ValueType) -> Value {
        self.signature.params.push(ty.clone());
        let param = self.entities.create_value(ValueData::Param {
            ty,
            index: self.signature.params.len() - 1,
        });
        self.entities.mark_param(param);
        param
    }
    pub fn set_return_type(&mut self, ty: ValueType) {
        self.signature.return_type = Some(ty);
    }
}

/// immutatble data getters.
impl Function {
    /// Get type of value.
    pub fn value_type(&self, value: Value) -> &ValueType {
        let value_data = self.entities.values.get(&value).unwrap();
        match value_data {
            ValueData::Inst { ty, .. } => ty,
            ValueData::Param { index, .. } => &self.signature.params[*index],
        }
    }
}
/// Data mutation for other entities.
impl Function {
    pub fn declar_external_function(&mut self, exfun_data: ExternalFunctionData) -> FunctionRef {
        let func_ref = FunctionRef(self.external_funcs.len() as u32);
        self.external_funcs.insert(func_ref, exfun_data);
        func_ref
    }
    pub fn declar_global_value(&mut self, global_data: GlobalValueData) -> GlobalValue {
        let global_value = GlobalValue(self.global_values.len() as u32);
        self.global_values.insert(global_value, global_data);
        global_value
    }
    pub fn declar_mem_type(&mut self, mem_type_data: MemTypeData) -> MemType {
        let mem_type = MemType(self.mem_type.len() as u32);
        self.mem_type.insert(mem_type, mem_type_data);
        mem_type
    }
    pub fn create_constant(&mut self, constant_data: ConstantData) -> Constant {
        let constant = Constant(self.constants.len() as u32);
        self.constants.insert(constant, constant_data);
        constant
    }
}
/// Data mutation to function block. inherit from `layout` or `entities`
/// or combine two structure to provide a more abstract interface.
impl Function {
    /// Create Block in function. append block in the end of function.
    pub fn create_block(&mut self) -> Block {
        let block = self.entities.create_block(BlockData {
            phis: Default::default(),
            insts: Default::default(),
        });
        self.layout.append_block(block);
        block
    }
    /// Create a block without appending it to function.
    pub fn create_block_only(&mut self) -> Block {
        self.entities.create_block(BlockData {
            phis: Default::default(),
            insts: Default::default(),
        })
    }
    /// Create remove a inst in layout.
    pub fn remove_inst(&mut self, inst: Instruction) {
        self.layout.remove_inst(inst);
    }
    /// replace a inst with given new inst data. callee must ensure instruction has
    /// already exit in function.
    pub fn replace_inst(&mut self, inst: Instruction, inst_data: InstructionData) {
        self.entities.insts.insert(inst, inst_data);
    }
}

/// Expose API from FunctionEntites.
impl Function {
    /// Inherit from `FunctionEntities`.
    pub fn get_block_data(&self, block: Block) -> &BlockData {
        self.entities.get_block_data(block)
    }
    /// Inherit from `FunctionEntities`.
    pub fn get_inst_data(&self, inst: Instruction) -> &InstructionData {
        self.entities.get_inst_data(inst)
    }
    /// Inherit from `FunctionEntities`.
    pub fn get_value_data(&self, value: Value) -> &ValueData {
        self.entities.get_value_data(value)
    }
    /// Iinherit from `FunctionEntities`.
    pub fn get_block_data_mut(&mut self, block: Block) -> &mut BlockData {
        self.entities.get_block_data_mut(block)
    }
    /// Inherit from `FunctionEntities`.
    pub fn get_inst_data_mut(&mut self, inst: Instruction) -> &mut InstructionData {
        self.entities.get_inst_data_mut(inst)
    }
    /// Inherit from `FunctionEntities`.
    pub fn get_value_data_mut(&mut self, value: Value) -> &mut ValueData {
        self.entities.get_value_data_mut(value)
    }
    /// Inherit from `FunctionEntities`.
    pub fn get_inst_result(&self, inst: Instruction) -> Option<Value> {
        self.entities.insts_result.get(&inst).cloned()
    }
}

/// Expose API from FunctionLayout.
impl Function {
    /// Inherit from `FunctionLayout`.
    pub fn blocks(&self) -> Vec<Block> {
        self.layout.blocks()
    }
    /// Inherit from `FunctionLayout`.
    pub fn insts(&self) -> Vec<Instruction> {
        self.layout.insts()
    }
    /// Inherit from `FunctionLayout`.
    pub fn first_block(&self) -> Option<Block> {
        self.layout.first_block()
    }
    /// Inherit from `FunctionLayout`.
    pub fn last_block(&self) -> Option<Block> {
        self.layout.last_block()
    }
    /// Inherit from `FunctionLayout`.
    pub fn get_block_of_inst(&self, inst: Instruction) -> Block {
        self.layout.get_block_of_inst(inst)
    }
    /// Inherit from `FunctionLayout`.
    pub fn get_insts_of_block(&self, block: Block) -> Vec<Instruction> {
        self.layout.get_insts_of_block(block)
    }
    /// Inherit from `FunctionLayout`.
    pub fn append_inst(&mut self, inst: Instruction, block: Block) {
        self.layout.append_inst(inst, block);
    }
    pub fn unshift_inst(&mut self, inst: Instruction, block: Block) {
        self.layout.unshift_inst(inst, block);
    }
    /// Inherit from `FunctionLayout`.
    pub fn insert_inst_before(&mut self, inst: Instruction, before: Instruction) {
        self.layout.insert_inst_before(inst, before);
    }
    /// Inherit from `FunctionLayout`.
    pub fn insert_inst_after(&mut self, inst: Instruction, after: Instruction) {
        self.layout.insert_inst_after(inst, after);
    }
}

/// Compsite method for block mutation
impl Function {
    /// Create block of given block data and insert this after the `after` block
    pub fn create_and_insert_block_after(&mut self, block_data: BlockData, after: Block) -> Block {
        let block = self.entities.create_block(block_data);
        self.layout.insert_block_after(block, after);
        block
    }
    /// Create block of given block data and insert this before the `before` block
    pub fn create_and_insert_block_before(&mut self, block_data: BlockData, before: Block) -> Block {
        let block = self.entities.create_block(block_data);
        self.layout.insert_block_before(block, before);
        block
    }
}
