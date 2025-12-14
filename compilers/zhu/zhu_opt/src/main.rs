use zhu_ir::formatter::format;
use zhu_ir::frontend::parse;

use zhu_opt::analysis::cfg::cfg_anylysis;
use zhu_opt::analysis::domtree::domtree_analysis;
use zhu_opt::analysis::liveness::liveness_analysis;
use zhu_opt::analysis::rpo::revrese_post_order_analysis;
use zhu_opt::analysis::use_chain::UseChainPass;

use zhu_opt::opt::mem2reg::mem2reg_pass;
use zhu_opt::AnalysisPass;

fn main() {
    let source = "
func mem_alloc_inst (reg0: i16, reg1: i16) {
block0:
  reg2 = stackalloc i16, size 32, align 8
  brif reg0 block1 block2
block1:
  store reg0 [reg2, 0]
  jump block3
block2:
  store reg1 [reg2, 0]
  jump block3
block3:
  reg3 = load i16 [reg2, 0]
  ret
}";
    let single_block_source = "
func mem_alloc_inst (reg0: i16, reg1: i16) {
block0:
  reg2 = stackalloc i16, size 32, align 8
  reg3 = add reg0 reg1
  store reg3 [reg2, 0]
  reg4 = load i16 [reg2, 0]
  reg5 = add reg4 reg1
  store reg5 [reg2, 0]
  ret
}
";

    let mut module = parse(single_block_source);
    let module_id = module.get_module_id_by_symbol("mem_alloc_inst").unwrap();
    let func_id = match module_id {
        zhu_ir::entities::module::ModuleLevelId::Func(func_id) => func_id,
        _ => panic!("Expected a function module ID"),
    };
    let function = module.get_mut_function(*func_id).unwrap();

    let mut use_pass = UseChainPass::new();
    let use_table = use_pass.process(&function);

    let cfg = cfg_anylysis(function);
    let dom = domtree_analysis(function, &cfg);
    let rpo = revrese_post_order_analysis(&cfg);
    let liveness = liveness_analysis(&function, &cfg, &rpo);

    let _pass = mem2reg_pass(function, &use_table, &liveness, &dom, &cfg, &rpo);

    println!("{}", format(&module));
}
