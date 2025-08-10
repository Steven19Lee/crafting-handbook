use zhu_ir::builder::FunctionBuilder;
use zhu_ir::entities::module::Module;
use zhu_ir::entities::r#type::ValueType;

pub fn build_module() -> Module {
    let mut module = Module::new();
    let func_id = module.declar_function("binary_inst_byte");
    let func_mut_refernece = module.get_mut_function(func_id).unwrap();

    let reg0 = func_mut_refernece.def_func_param(ValueType::U16);
    let reg1 = func_mut_refernece.def_func_param(ValueType::U16);

    let bb = func_mut_refernece.create_block();

    let mut builder = FunctionBuilder::new(func_mut_refernece);
    builder.switch_to_block(bb);
    let reg2 = builder.band_inst([reg1, reg0]);
    let reg3 = builder.bor_inst([reg2, reg1]);
    let reg4 = builder.shr_inst([reg3, reg2]);
    let _reg5 = builder.shl_inst([reg4, reg3]);
    builder.ret_inst(None);
    module
}
