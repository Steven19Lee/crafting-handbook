use crate::entities::block::Block;
use crate::entities::constant::{Constant, ConstantData};
use crate::entities::function::{Function, FunctionRef};
use crate::entities::global_value::GlobalValue;
use crate::entities::immediate::{Immediate, Offset};
use crate::entities::instruction::opcode::{CmpFlag, OpCode};
use crate::entities::instruction::{Instruction, InstructionData};
use crate::entities::r#type::{MemTypeData, ValueType};
use crate::entities::value::{Value, ValueData};

/// Builder interface to build instruction in a function.
pub struct FunctionBuilder<'a> {
    function: &'a mut Function,
    current_block: Option<Block>,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(func: &'a mut Function) -> Self {
        Self {
            function: func,
            current_block: None,
        }
    }
    pub fn switch_to_block(&mut self, block: Block) {
        self.current_block = Some(block);
    }
    fn get_current_block(&self) -> Block {
        self.current_block
            .expect("current block is not set, please call `switch_to_block` first")
    }
}
/// General function for building instruction in given function IR.
impl<'a> FunctionBuilder<'a> {
    pub fn build_inst_and_result_entities(
        &mut self,
        inst_data: InstructionData,
        ty: ValueType,
    ) -> (Value, Instruction) {
        let cur_block = self.get_current_block();
        // add instruction data into entities
        let inst = self.function.entities.create_inst(inst_data);
        self.function.entities.mark_inst_block(inst, cur_block);
        let result = self.function.entities.create_value(ValueData::Inst { inst, ty });
        // add result and inst relation
        self.function.entities.mark_inst_result(result, inst);
        (result, inst)
    }
    /// Shared function for `build_xxx_inst`, function, create information in both
    /// entities and layout.
    pub(crate) fn build_inst_and_result(&mut self, inst_data: InstructionData, ty: ValueType) -> Value {
        let cur_block = self.get_current_block();
        let (result, inst) = self.build_inst_and_result_entities(inst_data, ty);
        // add instruction into layout
        self.function.layout.append_inst(inst, cur_block);
        result
    }
    /// Shared function for `build_xxx_inst`, function, create information in both
    /// entities and layout.
    fn build_inst_without_result(&mut self, inst_data: InstructionData) -> Instruction {
        let cur_block = self.get_current_block();
        // add instruction data into entities
        let inst = self.function.entities.create_inst(inst_data);
        self.function.entities.mark_inst_block(inst, cur_block);
        // add instruction into layout
        self.function.layout.append_inst(inst, cur_block);
        inst
    }
    /// Build a convert function. wrap `build_inst_and_result`, only provide opcode and
    /// converted value as parameter.
    pub(crate) fn build_convert_inst(&mut self, opcode: OpCode, value: Value) -> Value {
        let unary_inst_data = InstructionData::Convert {
            opcode: opcode.clone(),
            src: value,
        };
        let target_ty = match opcode {
            OpCode::ToU8 => ValueType::U8,
            OpCode::ToU16 => ValueType::U16,
            OpCode::ToU32 => ValueType::U32,
            OpCode::ToU64 => ValueType::U64,
            OpCode::ToI16 => ValueType::I16,
            OpCode::ToI32 => ValueType::I32,
            OpCode::ToI64 => ValueType::I64,
            OpCode::ToF32 => ValueType::F32,
            OpCode::ToF64 => ValueType::F64,
            OpCode::ToAddress => {
                let unknow_ty = self.function.declar_mem_type(MemTypeData::Unknow);
                ValueType::Mem(unknow_ty)
            }
            _ => panic!(),
        };
        self.build_inst_and_result(unary_inst_data, target_ty)
    }
    /// Build a unary insttuction which instruction data is `InstructionData:Unary`,
    /// wrap `build_inst_and_result`. only provide opcode and unary operand as paramemter.
    pub(crate) fn build_unary_inst(&mut self, opcode: OpCode, arg: Value) -> Value {
        let unary_inst_data = InstructionData::Unary { opcode, value: arg };
        let ty = self.function.value_type(arg).clone();
        self.build_inst_and_result(unary_inst_data, ty)
    }
    /// Build a binary inst, which instruction data is `InstructionData:Binary`,
    /// wrap `build_inst_and_result`, only provide opcode and binary operand as paramemter.
    pub(crate) fn build_binary_inst(&mut self, opcode: OpCode, args: [Value; 2]) -> Value {
        // TODO: check args type is same or not.
        let binary_inst_data = InstructionData::Binary { opcode, args };
        let ty = self.function.value_type(args[0]).clone();
        self.build_inst_and_result(binary_inst_data, ty)
    }
    /// Build a binary imm inst, which instruction data is `InstructionData:BinaryI`,
    /// wrap `build_inst_and_result`, only provide opcode and binary operand as paramemter.
    pub(crate) fn build_binary_imm_inst(&mut self, opcode: OpCode, value: Value, imm: Immediate) -> Value {
        let binary_imm_inst_data = InstructionData::BinaryI { opcode, value, imm };
        let ty = self.function.value_type(value).clone();
        self.build_inst_and_result(binary_imm_inst_data, ty)
    }
    /// Build a const inst, which instruction data is `InstructionData:UnartConst`,
    /// wrap `build_inst_and_result`, only provide opcode and binary operand as paramemter.
    pub(crate) fn build_const_inst(&mut self, opcode: OpCode, bytes: Vec<u8>, ty: ValueType) -> Value {
        let constant_data = ConstantData { bytes };
        let constant = Constant(self.function.constants.keys().len() as u32);
        self.function.constants.insert(constant, constant_data);
        let const_inst_data = InstructionData::UnaryConst { opcode, constant };
        self.build_inst_and_result(const_inst_data, ty)
    }
}

///  Build Ret and Call instruction.
impl<'a> FunctionBuilder<'a> {
    /// Build ret instruction with optional value.
    ///
    /// Input :
    ///  - value: return value
    ///
    /// Output:
    ///  - a instruction reference of ret inst.
    pub fn ret_inst(&mut self, value: Option<Value>) -> Instruction {
        let inst_data = InstructionData::Ret {
            opcode: OpCode::Ret,
            value,
        };
        self.build_inst_without_result(inst_data)
    }
    /// Build call instruction with function ref and params
    ///
    /// Input :
    ///  - params: paramemters.
    ///  - func_ref: reference of function.
    ///
    /// Output:
    ///  - value if given function signature return type is not none, otherwise is none.
    pub fn call_inst(&mut self, params: Vec<Value>, func_ref: FunctionRef) -> Option<Value> {
        let inst_data = InstructionData::Call {
            opcode: OpCode::Call,
            name: func_ref.clone(),
            params,
        };
        let inst = self.build_inst_without_result(inst_data);
        // insert result is return type is not none.
        let exfun = self.function.external_funcs.get(&func_ref).unwrap();
        let sig = &exfun.sig;
        if let Some(ty) = &sig.return_type {
            let result = self
                .function
                .entities
                .create_value(ValueData::Inst { inst, ty: ty.clone() });
            println!("??");
            self.function.entities.mark_inst_result(result, inst);
            Some(result)
        } else {
            None
        }
    }
}
/// Build convert instruction
impl<'a> FunctionBuilder<'a> {
    /// Build convert u8 instruction.
    ///
    /// Input:
    ///   - src: target value need to convert.
    ///
    /// Output:
    ///   - a converted value with type `U8`.
    pub fn to_u8_inst(&mut self, src: Value) -> Value {
        self.build_convert_inst(OpCode::ToU8, src)
    }
    /// Build convert u16 instruction.
    ///
    /// Input:
    ///   - src: target value need to convert.
    ///
    /// Output:
    ///   - a converted value with type `U16`.
    pub fn to_u16_inst(&mut self, src: Value) -> Value {
        self.build_convert_inst(OpCode::ToU16, src)
    }
    /// Build convert u32 instruction.
    ///
    /// Input:
    ///   - src: target value need to convert.
    ///
    /// Output:
    ///   - a converted value with type `U32`.
    pub fn to_u32_inst(&mut self, src: Value) -> Value {
        self.build_convert_inst(OpCode::ToU32, src)
    }
    /// Build convert u64 instruction.
    ///
    /// Input:
    ///   - src: target value need to convert.
    ///
    /// Output:
    ///   - a converted value with type `U64`.
    pub fn to_u64_inst(&mut self, src: Value) -> Value {
        self.build_convert_inst(OpCode::ToU64, src)
    }
    /// Build convert i16 instruction.
    ///
    /// Input:
    ///   - src: target value need to convert.
    ///
    /// Output:
    ///   - a converted value with type `I16`.
    pub fn to_i16_inst(&mut self, src: Value) -> Value {
        self.build_convert_inst(OpCode::ToI16, src)
    }
    /// Build convert i32 instruction.
    ///
    /// Input:
    ///   - src: target value need to convert.
    ///
    /// Output:
    ///   - a converted value with type `I32`.
    pub fn to_i32_inst(&mut self, src: Value) -> Value {
        self.build_convert_inst(OpCode::ToI32, src)
    }
    /// Build convert i64 instruction.
    ///
    /// Input:
    ///   - src: target value need to convert.
    ///
    /// Output:
    ///   - a converted value with type `I64`.
    pub fn to_i64_inst(&mut self, src: Value) -> Value {
        self.build_convert_inst(OpCode::ToI64, src)
    }
    /// Build convert f32 instruction.
    ///
    /// Input:
    ///   - src: target value need to convert.
    ///
    /// Output:
    ///   - a converted value with type `F32`.
    pub fn to_f32_inst(&mut self, src: Value) -> Value {
        self.build_convert_inst(OpCode::ToF32, src)
    }
    /// Build convert f64 instruction.
    ///
    /// Input:
    ///   - src: target value need to convert.
    ///
    /// Output:
    ///   - a converted value with type `F64`.
    pub fn to_f64_inst(&mut self, src: Value) -> Value {
        self.build_convert_inst(OpCode::ToF64, src)
    }
    /// Build convert address instruction.
    ///
    /// Input:
    ///   - src: target value need to convert.
    ///
    /// Output:
    ///   - a converted value with type `address`.
    pub fn to_address_inst(&mut self, src: Value) -> Value {
        self.build_convert_inst(OpCode::ToAddress, src)
    }
}
/// Build conditional branch or unconditional branch instruction
impl<'a> FunctionBuilder<'a> {
    /// Build brif instruction
    ///
    /// Input:
    ///   - test: test virtual register if bigger than 0.
    ///   - conseq: branch block if test value is bigger than 0.
    ///   - alter: branch block if test valie is <= 0.
    ///
    /// Output:
    ///   - None.
    pub fn brif_inst(&mut self, test: Value, conseq: Block, alter: Block) {
        let inst_data = InstructionData::BrIf {
            opcode: OpCode::BrIf,
            test,
            conseq,
            alter,
        };
        self.build_inst_without_result(inst_data);
    }
    /// Buold jump instruction
    ///
    /// Input:
    ///   - block: the target block need to jump to.
    ///
    /// Output:
    ///   - None.
    pub fn jump_inst(&mut self, block: Block) {
        let inst_data = InstructionData::Jump {
            opcode: OpCode::Jump,
            dst: block,
        };
        self.build_inst_without_result(inst_data);
    }
}
/// Build Memory relative instruction
impl<'a> FunctionBuilder<'a> {
    /// Build stackalloc instruction
    pub fn stack_alloc_inst(&mut self, size: Immediate, align: Immediate, ty: ValueType) -> Value {
        let inst_data = InstructionData::StackAlloc {
            opcode: OpCode::StackAlloc,
            size,
            align,
        };
        self.build_inst_and_result(inst_data, ty)
    }
    /// Build load instruction
    ///
    /// Input :
    /// - base: base register contain address
    /// - offset: constant offset from base register.
    /// - ty: what kind of type the loaded data should be
    ///
    /// Output:
    /// - register point contain loaded data.
    ///  
    pub fn load_inst(&mut self, base: Value, offset: Offset, ty: ValueType) -> Value {
        let inst_data = InstructionData::LoadRegister {
            opcode: OpCode::LoadRegister,
            base,
            offset,
        };
        self.build_inst_and_result(inst_data, ty)
    }
    /// Build a store instruction
    ///
    /// Input:
    /// - base: base register contain address
    /// - offset: constant offset from base register.
    /// - src: data source to store in memory.
    ///
    /// Output:
    /// - None
    pub fn store_inst(&mut self, base: Value, offset: Offset, src: Value) {
        let inst_data = InstructionData::StoreRegister {
            opcode: OpCode::StoreRegister,
            base,
            offset,
            src,
        };
        self.build_inst_without_result(inst_data);
    }
    /// Build a global load instruction
    ///
    pub fn global_load_inst(&mut self, base: GlobalValue, offset: Offset, ty: ValueType) -> Value {
        let inst_data = InstructionData::GlobalLoad {
            opcode: OpCode::GlobalLoad,
            base,
            offset,
        };
        self.build_inst_and_result(inst_data, ty)
    }
    pub fn global_store_inst(&mut self, base: GlobalValue, offset: Offset, src: Value) {
        let inst_data = InstructionData::GlobalStore {
            opcode: OpCode::GlobalStore,
            base,
            offset,
            src,
        };
        self.build_inst_without_result(inst_data);
    }
}

impl<'a> FunctionBuilder<'a> {
    /// Build mov instruction.
    ///
    /// Input :
    ///  - value: source value
    ///
    /// Output:
    ///  - a copy of origin value.
    pub fn mov_inst(&mut self, value: Value) -> Value {
        self.build_unary_inst(OpCode::Mov, value)
    }
    /// Build neg instruction.
    ///
    /// Input :
    ///  - value: source value
    ///
    /// Output:
    ///  - a neg of origin value.
    pub fn neg_inst(&mut self, value: Value) -> Value {
        self.build_unary_inst(OpCode::Neg, value)
    }
    /// Build neg instruction.
    ///
    /// Input :
    ///  - value: source value
    ///
    /// Output:
    ///  - a not of origin value.
    pub fn bnot_inst(&mut self, value: Value) -> Value {
        self.build_unary_inst(OpCode::BitwiseNot, value)
    }
}
impl<'a> FunctionBuilder<'a> {
    /// Build instructuon generate sign int constant.
    ///
    /// Input :
    ///  - bytes: constant data in bytes
    ///  - ty: Value type of constant data
    ///
    /// Output:
    ///  - a Value with `ty` value type.
    ///
    /// Invariant:
    ///   - caller need to make sure `ty` is one of sign int
    ///   - caller need to make sure `bytes` format.
    pub fn iconst_inst(&mut self, bytes: Vec<u8>, ty: ValueType) -> Value {
        self.build_const_inst(OpCode::Iconst, bytes, ty)
    }
    /// Build instruction generate f32 constant.
    ///
    /// Input :
    ///  - bytes: constant data in bytes
    ///  - ty: Value type of constant data
    ///
    /// Output:
    ///  - a Value with `ty` value type.
    ///
    /// Invariant:
    ///   - caller need to make sure `bytes` format.
    pub fn fconst_inst(&mut self, bytes: Vec<u8>, ty: ValueType) -> Value {
        self.build_const_inst(OpCode::Fconst, bytes, ty)
    }
}
impl<'a> FunctionBuilder<'a> {
    /// Build icmp instruction to compare args with flags.
    ///
    /// Input :
    ///  - flag: the operator of compare.
    ///  - args: two value to compare, format is `arg1 <op> arg2`
    ///
    /// Output:
    ///  - a Value which type is U8, only have two possible value, 0 and 1.
    pub fn icmp_inst(&mut self, flag: CmpFlag, args: [Value; 2]) -> Value {
        let inst_data = InstructionData::Icmp {
            opcode: OpCode::Icmp,
            flag,
            args,
        };
        self.build_inst_and_result(inst_data, ValueType::U8)
    }
    /// Build fcmp instruction to compare args with flags.
    ///
    /// Input :
    ///  - flag: the operator of compare.
    ///  - args: two value to compare, format is `arg1 <op> arg2`
    ///
    /// Output:
    ///  - a Value which type is U8, only have two possible value, 0 and 1.
    pub fn fcmp_inst(&mut self, flag: CmpFlag, args: [Value; 2]) -> Value {
        let inst_data = InstructionData::Icmp {
            opcode: OpCode::Fcmp,
            flag,
            args,
        };
        self.build_inst_and_result(inst_data, ValueType::U8)
    }
}
/// Build binary instruction.
impl<'a> FunctionBuilder<'a> {
    /// Build Add instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn add_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::Add, args)
    }
    /// Build Sub instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn sub_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::Sub, args)
    }
    /// Build Mul instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn mul_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::Mul, args)
    }
    /// Build Divide instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn divide_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::Divide, args)
    }
    /// Build Reminder instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn reminder_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::Reminder, args)
    }
    /// Build Fadd instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn fadd_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::FAdd, args)
    }
    /// Build Fsub instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn fsub_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::FSub, args)
    }
    /// Build Fmul instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn fmul_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::FMul, args)
    }
    /// Build Fdivide instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn fdivide_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::FDivide, args)
    }
    /// Build Freminder instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn freminder_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::FReminder, args)
    }
    /// Build Bor (Bitwise or) instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn bor_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::BitwiseOR, args)
    }
    /// Build Bor (Bitwise adn) instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn band_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::BitwiseAnd, args)
    }
    /// Build shit left instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn shl_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::ShiftLeft, args)
    }
    /// Build shit right instruction
    ///
    /// Input
    ///  - args: Operand of instruction
    ///
    /// Output:
    ///  - result value
    pub fn shr_inst(&mut self, args: [Value; 2]) -> Value {
        self.build_binary_inst(OpCode::ShiftRight, args)
    }
}
/// Build binary imm instruction
impl<'a> FunctionBuilder<'a> {
    pub fn add_imm_inst(&mut self, value: Value, imm: Immediate) -> Value {
        self.build_binary_imm_inst(OpCode::Addi, value, imm)
    }
    pub fn sub_imm_inst(&mut self, value: Value, imm: Immediate) -> Value {
        self.build_binary_imm_inst(OpCode::Subi, value, imm)
    }
    pub fn mul_imm_inst(&mut self, value: Value, imm: Immediate) -> Value {
        self.build_binary_imm_inst(OpCode::Muli, value, imm)
    }
    pub fn divide_imm_inst(&mut self, value: Value, imm: Immediate) -> Value {
        self.build_binary_imm_inst(OpCode::Dividei, value, imm)
    }
    pub fn reminder_imm_inst(&mut self, value: Value, imm: Immediate) -> Value {
        self.build_binary_imm_inst(OpCode::Reminderi, value, imm)
    }
}

impl<'a> FunctionBuilder<'a> {
    pub fn phi_inst(&mut self, from: Vec<(Block, Value)>) -> Value {
        let block = self.get_current_block();
        let ty = self.function.value_type(from[0].1).clone();
        let inst = self.function.entities.create_inst(InstructionData::Phi {
            opcode: OpCode::Phi,
            from,
        });
        let result = self.function.entities.create_value(ValueData::Inst { inst, ty });
        self.function.entities.mark_phi_block(inst, block);
        self.function.entities.mark_inst_result(result, inst);
        self.function
            .layout
            .unshift_inst(inst, self.current_block.clone().unwrap());
        result
    }
}
