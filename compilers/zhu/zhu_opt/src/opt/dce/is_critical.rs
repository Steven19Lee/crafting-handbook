use zhu_ir::entities::instruction::InstructionData;

/// Is instruction is critial ? (contain side effect)
///
/// NOTE: there we do not consider trappable instruction
/// as critial like divide inst (divide with 0 should trigger
/// a exception), just for make IR more simple.
pub fn is_critical_inst(inst_data: &InstructionData) -> bool {
    match inst_data {
        InstructionData::LoadRegister { .. }
        | InstructionData::GlobalLoad { .. }
        | InstructionData::StoreRegister { .. }
        | InstructionData::GlobalStore { .. }
        | InstructionData::Call { .. }
        | InstructionData::Ret { .. } => true,
        _ => false,
    }
}
