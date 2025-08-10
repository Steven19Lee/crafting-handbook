use zhu_ir::entities::immediate::Immediate;
use zhu_ir::entities::immediate::Offset;
use zhu_ir::entities::r#type::ValueType;
use zhu_ir::{builder::FunctionBuilder, entities::module::Module};

const FUNC_NAME: &'static str = "mem_alloc_inst";

pub fn build_module() -> Module {
    let mut module = Module::new();
    let func_id = module.declar_function(FUNC_NAME);
    let func_mut_ref = module.get_mut_function(func_id).unwrap();
    let reg0 = func_mut_ref.def_func_param(ValueType::I16);
    let bb = func_mut_ref.create_block();
    let mut builder = FunctionBuilder::new(func_mut_ref);
    builder.switch_to_block(bb);
    let reg1 = builder.stack_alloc_inst(Immediate::U32(32), Immediate::U8(8), ValueType::I16);
    builder.store_inst(reg1, Offset(0), reg0);
    builder.ret_inst(None);
    module
}
