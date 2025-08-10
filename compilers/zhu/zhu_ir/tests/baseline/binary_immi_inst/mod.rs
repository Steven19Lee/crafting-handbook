use zhu_ir::builder::FunctionBuilder;
use zhu_ir::entities::immediate::Immediate;
use zhu_ir::entities::module::Module;
use zhu_ir::entities::r#type::ValueType;

pub fn build_module() -> Module {
    let mut module = Module::new();
    let func_id = module.declar_function("binary_immi_inst");
    let func_mut_refernece = module.get_mut_function(func_id).unwrap();

    let _reg0 = func_mut_refernece.def_func_param(ValueType::U16);
    let reg1 = func_mut_refernece.def_func_param(ValueType::U16);

    let bb = func_mut_refernece.create_block();

    let mut builder = FunctionBuilder::new(func_mut_refernece);
    builder.switch_to_block(bb);
    let reg2 = builder.add_imm_inst(reg1, Immediate::U16(5));
    let reg3 = builder.sub_imm_inst(reg2, Immediate::U16(10));
    let reg4 = builder.mul_imm_inst(reg3, Immediate::U16(15));
    let reg5 = builder.divide_imm_inst(reg4, Immediate::U16(20));
    let _reg6 = builder.reminder_imm_inst(reg5, Immediate::U16(25));
    builder.ret_inst(None);
    module
}
