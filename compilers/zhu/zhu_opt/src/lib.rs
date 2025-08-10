pub mod analysis;
pub mod opt;

use zhu_ir::entities::{function::Function, module::Module};
/// Trait for a pass which will mutate a function to get opti function.
pub trait OptiPass {
    fn process(&mut self, func: &mut Function);
}
/// Trait for a pass which is analysis based on function, maybe return a
/// object for analysis.
pub trait AnalysisPass<T> {
    fn process(&mut self, func: &Function) -> T;
}

/// Trait for print anaylsis table
pub trait FormatTable {
    fn format_table(&self, func: &Function, module: &Module) -> String;
}

const HEADER_CHAR: char = '=';
const HEADER_LEN: usize = 10;

pub fn get_table_header(name: &str) -> String {
    format!("{HEADER_CHAR:=>HEADER_LEN$} {} {HEADER_CHAR:=>HEADER_LEN$}\n", name)
}
