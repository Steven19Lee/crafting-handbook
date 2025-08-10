use zhu_ir::builder::FunctionBuilder;
use zhu_ir::entities::instruction::opcode::CmpFlag;
use zhu_ir::entities::module::Module;
use zhu_ir::entities::r#type::ValueType;

pub fn build_module() -> Module {
    let mut module = Module::new();
    let func_id = module.declar_function("cmp_inst");
    let func_mut_refernece = module.get_mut_function(func_id).unwrap();

    let reg0 = func_mut_refernece.def_func_param(ValueType::U8);
    let reg1 = func_mut_refernece.def_func_param(ValueType::U8);
    let reg2 = func_mut_refernece.def_func_param(ValueType::F32);
    let reg3 = func_mut_refernece.def_func_param(ValueType::F32);

    let bb = func_mut_refernece.create_block();

    let mut builder = FunctionBuilder::new(func_mut_refernece);
    builder.switch_to_block(bb);
    builder.icmp_inst(CmpFlag::Eq, [reg0, reg1]);
    builder.icmp_inst(CmpFlag::NotEq, [reg0, reg1]);
    builder.icmp_inst(CmpFlag::Gt, [reg0, reg1]);
    builder.icmp_inst(CmpFlag::Gteq, [reg0, reg1]);
    builder.icmp_inst(CmpFlag::Lt, [reg0, reg1]);
    builder.icmp_inst(CmpFlag::LtEq, [reg0, reg1]);
    builder.fcmp_inst(CmpFlag::Eq, [reg2, reg3]);
    builder.fcmp_inst(CmpFlag::NotEq, [reg2, reg3]);
    builder.fcmp_inst(CmpFlag::Gt, [reg2, reg3]);
    builder.fcmp_inst(CmpFlag::Gteq, [reg2, reg3]);
    builder.fcmp_inst(CmpFlag::Lt, [reg2, reg3]);
    builder.fcmp_inst(CmpFlag::LtEq, [reg2, reg3]);
    builder.ret_inst(None);
    module
}
