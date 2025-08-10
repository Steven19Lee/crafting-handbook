use zhu_ir::entities::immediate::Offset;
use zhu_ir::entities::r#type::ValueType;
use zhu_ir::{
    builder::FunctionBuilder,
    entities::module::{DataDescription, Module},
};

const DATA_NAME: &'static str = "global_data";
const FUNC_NAME: &'static str = "global_inst";

pub fn build_module() -> Module {
    let mut module = Module::new();
    let data_id = module.define_data(DATA_NAME, DataDescription::new());
    let func_id = module.declar_function(FUNC_NAME);
    let global_value = module.declar_data_in_function(data_id, func_id);
    let func_mut_ref = module.get_mut_function(func_id).unwrap();
    let reg0 = func_mut_ref.def_func_param(ValueType::U8);
    let bb = func_mut_ref.create_block();
    let mut builder = FunctionBuilder::new(func_mut_ref);
    builder.switch_to_block(bb);
    let reg1 = builder.global_load_inst(global_value, Offset(0), ValueType::U8);
    let reg2 = builder.add_inst([reg0, reg1]);
    builder.global_store_inst(global_value, Offset(0), reg2);
    builder.ret_inst(None);
    module
}
