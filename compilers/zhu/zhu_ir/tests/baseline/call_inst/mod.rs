use zhu_ir::builder::FunctionBuilder;
use zhu_ir::entities::module::Module;
use zhu_ir::entities::r#type::ValueType;

const CALL_INST_FUNC_NAME: &'static str = "call_inst";
const RETURN_U8_NAME: &'static str = "return_u8";
const RETURN_NONE_NAME: &'static str = "return_none";
const RETURN_U8_MULTI_NAME: &'static str = "return_u8_multi_arguments";

pub fn build_module() -> Module {
    let mut module = Module::new();
    let (return_u8_func_param, return_u8_func) = {
        let func_id = module.declar_function(RETURN_U8_NAME);
        let func_mut_ref = module.get_mut_function(func_id).unwrap();
        let reg0 = func_mut_ref.def_func_param(ValueType::U8);
        func_mut_ref.set_return_type(ValueType::U8);
        ([reg0], func_id)
    };
    let (return_u8_multi_param, return_u8_multi) = {
        let func_id = module.declar_function(RETURN_U8_MULTI_NAME);
        let func_mut_ref = module.get_mut_function(func_id).unwrap();
        let reg0 = func_mut_ref.def_func_param(ValueType::U8);
        let reg1 = func_mut_ref.def_func_param(ValueType::U8);
        func_mut_ref.set_return_type(ValueType::U8);
        ([reg0, reg1], func_id)
    };
    let return_none = module.declar_function(RETURN_NONE_NAME);
    let (call_inst_func_params, call_inst_func) = {
        let func_id = module.declar_function(CALL_INST_FUNC_NAME);
        let func_mut_ref = module.get_mut_function(func_id).unwrap();
        let reg0 = func_mut_ref.def_func_param(ValueType::U8);
        let reg1 = func_mut_ref.def_func_param(ValueType::U8);
        ([reg0, reg1], func_id)
    };
    // call inst func body
    {
        let return_u8_func_ref = module.declar_function_in_function(return_u8_func, call_inst_func);
        let return_none_func_ref = module.declar_function_in_function(return_none, call_inst_func);
        let return_u8_multi_func_ref = module.declar_function_in_function(return_u8_multi, call_inst_func);
        let func_mut_ref = module.get_mut_function(call_inst_func).unwrap();
        let [reg0, reg1] = call_inst_func_params;
        let block0 = func_mut_ref.create_block();
        let mut buildr = FunctionBuilder::new(func_mut_ref);
        buildr.switch_to_block(block0);
        buildr.call_inst(vec![reg0], return_u8_func_ref);
        buildr.call_inst(vec![], return_none_func_ref);
        buildr.call_inst(vec![reg0, reg1], return_u8_multi_func_ref);
        buildr.ret_inst(None);
    }
    // return none
    {
        let func_mut_ref = module.get_mut_function(return_none).unwrap();
        let block0 = func_mut_ref.create_block();
        let mut buildr = FunctionBuilder::new(func_mut_ref);
        buildr.switch_to_block(block0);
        buildr.ret_inst(None);
    }
    // return u8
    {
        let func_mut_ref = module.get_mut_function(return_u8_func).unwrap();
        let [reg0] = return_u8_func_param;
        func_mut_ref.set_return_type(ValueType::U8);
        let block0 = func_mut_ref.create_block();
        let mut buildr = FunctionBuilder::new(func_mut_ref);
        buildr.switch_to_block(block0);
        buildr.ret_inst(Some(reg0));
    }
    // return u8 multi argument
    {
        let func_mut_ref = module.get_mut_function(return_u8_multi).unwrap();
        let [reg0, reg1] = return_u8_multi_param;
        func_mut_ref.set_return_type(ValueType::U8);
        let block0 = func_mut_ref.create_block();
        let mut buildr = FunctionBuilder::new(func_mut_ref);
        buildr.switch_to_block(block0);
        let reg2 = buildr.add_inst([reg0, reg1]);
        buildr.ret_inst(Some(reg2));
    }
    module
}
