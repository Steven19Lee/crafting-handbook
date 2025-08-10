use zhu_ir::entities::immediate::Immediate;
use zhu_ir::entities::immediate::Offset;
use zhu_ir::entities::r#type::ValueType;
use zhu_ir::{builder::FunctionBuilder, entities::module::Module};

const FUNC_NAME: &'static str = "mem_inst_base";

pub fn build_module() -> Module {
    let mut module = Module::new();
    let func_id = module.declar_function(FUNC_NAME);
    let func_mut_ref = module.get_mut_function(func_id).unwrap();
    let reg0 = func_mut_ref.def_func_param(ValueType::U8);
    let bb = func_mut_ref.create_block();
    let mut builder = FunctionBuilder::new(func_mut_ref);
    builder.switch_to_block(bb);
    let reg1 = builder.load_inst(reg0, Offset(0), ValueType::I16);
    let reg2 = builder.add_imm_inst(reg1, Immediate::I16(10));
    builder.store_inst(reg0, Offset(0), reg2);
    builder.ret_inst(None);
    module
}
