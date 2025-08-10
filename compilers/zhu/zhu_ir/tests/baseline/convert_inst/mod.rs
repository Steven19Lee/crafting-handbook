use zhu_ir::builder::FunctionBuilder;
use zhu_ir::entities::module::Module;
use zhu_ir::entities::r#type::ValueType;

pub fn build_module() -> Module {
    let mut module = Module::new();
    let func_id = module.declar_function("convert_inst");
    let func_mut_refernece = module.get_mut_function(func_id).unwrap();

    let reg0 = func_mut_refernece.def_func_param(ValueType::U8);

    let bb = func_mut_refernece.create_block();

    let mut builder = FunctionBuilder::new(func_mut_refernece);
    builder.switch_to_block(bb);
    let reg1 = builder.to_u8_inst(reg0);
    let reg2 = builder.to_u16_inst(reg1);
    let reg3 = builder.to_u32_inst(reg2);
    let reg4 = builder.to_u64_inst(reg3);
    let reg5 = builder.to_i16_inst(reg4);
    let reg6 = builder.to_i32_inst(reg5);
    let reg7 = builder.to_i64_inst(reg6);
    let reg8 = builder.to_f32_inst(reg7);
    let reg9 = builder.to_f64_inst(reg8);
    let _reg10 = builder.to_address_inst(reg9);
    builder.ret_inst(None);
    module
}
