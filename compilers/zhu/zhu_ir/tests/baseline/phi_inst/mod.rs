use zhu_ir::builder::FunctionBuilder;
use zhu_ir::entities::instruction::opcode::CmpFlag;
use zhu_ir::entities::module::Module;
use zhu_ir::entities::r#type::ValueType;

const FUNC_NAME: &'static str = "phi_inst";

pub fn build_module() -> Module {
    let mut module = Module::new();
    let func_id = module.declar_function(FUNC_NAME);
    let func_mut_refernece = module.get_mut_function(func_id).unwrap();

    let reg0 = func_mut_refernece.def_func_param(ValueType::U8);
    let reg1 = func_mut_refernece.def_func_param(ValueType::U8);

    let b0 = func_mut_refernece.create_block();
    let b1 = func_mut_refernece.create_block();
    let b2 = func_mut_refernece.create_block();
    let b3 = func_mut_refernece.create_block();

    let mut builder = FunctionBuilder::new(func_mut_refernece);

    builder.switch_to_block(b0);
    let reg2 = builder.icmp_inst(CmpFlag::Eq, [reg0, reg1]);
    builder.brif_inst(reg2, b1, b2);

    builder.switch_to_block(b1);
    let reg3 = builder.add_inst([reg0, reg1]);
    builder.jump_inst(b3);

    builder.switch_to_block(b2);
    let reg4 = builder.sub_inst([reg0, reg1]);
    builder.jump_inst(b3);

    builder.switch_to_block(b3);
    builder.phi_inst(vec![(b1, reg3), (b2, reg4)]);
    builder.ret_inst(None);
    module
}
