use zhu_ir::builder::FunctionBuilder;
use zhu_ir::entities::immediate::{Immediate, Offset};
use zhu_ir::entities::module::Module;
use zhu_ir::entities::r#type::{MemTypeData, StructTypeData, StructTypeDataField, ValueType};

const FUNC_NAME: &'static str = "mem_inst_struct";

pub fn build_module() -> Module {
    let mut module = Module::new();

    let func_id = module.declar_function(FUNC_NAME);
    let func_mut_reference = module.get_mut_function(func_id).unwrap();
    func_mut_reference.set_return_type(ValueType::I16);
    let mem_type_0 = func_mut_reference.declar_mem_type(MemTypeData::Struct(StructTypeData {
        size: 32,
        fields: vec![
            StructTypeDataField {
                offset: 0,
                ty: ValueType::I16,
            },
            StructTypeDataField {
                offset: 16,
                ty: ValueType::I16,
            },
        ],
    }));
    let mem_type_1 = func_mut_reference.declar_mem_type(MemTypeData::Struct(StructTypeData {
        size: 48,
        fields: vec![
            StructTypeDataField {
                offset: 0,
                ty: ValueType::I16,
            },
            StructTypeDataField {
                offset: 16,
                ty: ValueType::Mem(mem_type_0),
            },
        ],
    }));
    let bb = func_mut_reference.create_block();
    let mut builder = FunctionBuilder::new(func_mut_reference);
    builder.switch_to_block(bb);
    // build mem type 0
    let reg0 = builder.stack_alloc_inst(Immediate::U32(32), Immediate::U8(8), ValueType::Mem(mem_type_0));
    let reg1 = builder.load_inst(reg0, Offset(0), ValueType::I16);
    let reg2 = builder.load_inst(reg0, Offset(16), ValueType::I16);
    let reg3 = builder.add_inst([reg1, reg2]);
    // build mem type 1
    let reg4 = builder.stack_alloc_inst(Immediate::U32(32), Immediate::U8(8), ValueType::Mem(mem_type_1));
    let reg5 = builder.load_inst(reg4, Offset(0), ValueType::I16);
    let reg6 = builder.load_inst(reg4, Offset(32), ValueType::I16);
    let reg7 = builder.add_inst([reg5, reg6]);
    // build result
    let reg8 = builder.add_inst([reg3, reg7]);
    builder.ret_inst(Some(reg8));

    module
}
