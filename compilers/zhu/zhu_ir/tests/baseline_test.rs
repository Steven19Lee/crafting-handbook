#![feature(macro_metavar_expr_concat)]
mod baseline;

use std::env::current_dir;
use std::fs::read_to_string;
use std::path::PathBuf;
use zhu_ir::builder::FunctionBuilder;
use zhu_ir::entities::module::ModuleLevelId;
use zhu_ir::entities::r#type::ValueType;
use zhu_ir::formatter::format;
use zhu_ir::frontend::parse;

fn get_baseline_folder_path() -> PathBuf {
    current_dir().unwrap().join("tests/baseline")
}

fn read_file_from_case_name(name: &str) -> String {
    let path_buf = get_baseline_folder_path().join(name).join("case.zhu");
    read_to_string(path_buf)
        .unwrap_or_else(|_| panic!("[Error]: test file can not read. path. namespace is {:?}", name))
}

macro_rules! generate_test_cases {
    ( $($test_case: ident),* ) => {
        $(
            #[test]
            fn ${concat($test_case, _buildable)}() {
                baseline::$test_case::build_module();
            }
            #[test]
            fn ${concat($test_case, _parseable)}() {
                let source = read_file_from_case_name(stringify!($test_case));
                parse(&source);
            }
            #[test]
            fn ${concat($test_case, _parse_match_formatter)}() {
                let source = read_file_from_case_name(stringify!($test_case));
                let module = parse(&source);
                let result = format(&module);
                assert_eq!(result, source);
            }
            #[test]
            fn ${concat($test_case, _build_match_formatter)}() {
                let module = baseline::$test_case::build_module();
                let result = format(&module);
                let source = read_file_from_case_name(stringify!($test_case));
                assert_eq!(result, source);
            }
        )*
    };
}

generate_test_cases! {
    convert_inst,
    unary_inst,
    cmp_inst,
    call_inst,
    branch_inst,
    binary_inst_base,
    binary_inst_byte,
    binary_inst_float,
    binary_immi_inst,
    global_inst,
    mem_inst_base,
    mem_alloc_inst,
    mem_inst_struct,
    phi_inst
}

#[test]
/// Some function after optimization, will remove instruction and value from layout, so after parse
/// given function, we need to reset the max index of block and value.
fn when_reg_and_block_index_is_not_continue_module_create_by_parser_can_reset_the_index_correctly() {
    let source = read_file_from_case_name("correct_module_after_parse");
    let mut module = parse(&source);
    let func_name = "correct_module_after_parse";

    let id = match module.get_module_id_by_symbol(func_name).unwrap() {
        ModuleLevelId::Func(func) => func.clone(),
        _ => panic!(),
    };
    let func = module.get_mut_function(id).unwrap();
    let bb = func.create_block();
    let mut builder = FunctionBuilder::new(func);
    builder.switch_to_block(bb);
    builder.iconst_inst(vec![0], ValueType::I32);

    let result = format(&module);

    assert_eq!(
        result,
        read_to_string(
            get_baseline_folder_path()
                .join("correct_module_after_parse")
                .join("expect.zhu")
        )
        .unwrap()
    )
}
