use crate::builder::FunctionBuilder;
use crate::entities::block::Block;
use crate::entities::external_name::ExternalName;
use crate::entities::external_name::UserDefNamespace;
use crate::entities::function::ExternalFunctionData;
use crate::entities::function::Function;
use crate::entities::global_value::{GlobalValue, GlobalValueData};
use crate::entities::immediate::Immediate;
use crate::entities::immediate::Offset;
use crate::entities::module::DataDescription;
use crate::entities::module::Module;
use crate::entities::module::ModuleLevelId;
use crate::entities::r#type::MemType;
use crate::entities::r#type::StructTypeData;
use crate::entities::r#type::StructTypeDataField;
use crate::entities::r#type::{MemTypeData, ValueType};
use crate::entities::value::Value;
use crate::entities::value::ValueData;
use crate::frontend::utils::{map_token_to_cmp, map_token_to_opcode};
use crate::frontend::Lexer;
use crate::frontend::TokenKind;
use std::str::FromStr;
/// Parser for Zhu IR, parse text format and create
/// a in memory module
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    module: Module,
    function: Function,
    block: Block,
}
/// Panic when current token is unexpected.
macro_rules! unexpect_token {
    ($lexer: expr) => {
        panic!(
            "[Error]: Unexpect token kind {:?} ({}, {}).",
            $lexer.get_token_kind(),
            $lexer.get_start_pos(),
            $lexer.get_end_pos()
        )
    };
}
/// Expect a token, call next token if match,
/// otherwise will panic.
macro_rules! expect_token {
    ($lexer: expr, $kind: expr) => {
        if $lexer.get_token_kind() == $kind {
            $lexer.next_token();
        } else {
            unexpect_token!($lexer)
        }
    };
}
/// Return true when match given token kind or EOF,
/// otherwise return false, use it when match in
/// loop.
macro_rules! match_tokens {
    ($lexer: expr, $($token:pat), *) => {
        {
            let cur_kind = $lexer.get_token_kind();
            if matches!(cur_kind, $($token)|*)  {
                true
            }else {
                false
            }
        }
    };
}
/// Parse Identifier, not using function because function call will
/// break lifetime.
macro_rules! parse_identifier {
    ($lexer: expr) => {{
        let id_str = $lexer.get_source_string();
        expect_token!($lexer, TokenKind::Identifier);
        id_str
    }};
}
/// Helper marco for `parse_immediate_by_value_type`
/// function.
macro_rules! parse_immediate_helper_marco {
    ($parser: expr, $ty: ty) => {
        match $parser.lexer.get_token_kind() {
            TokenKind::DecimalString => $parser.parse_decimal_string::<$ty>(),
            TokenKind::HexString => $parser.parse_hex_string::<$ty>(),
            _ => unexpect_token!($parser.lexer),
        }
    };
}
/// Since our IR strcuture is not define as syntax tree, some of
/// method in recursive parser will not return IR entity, it will
/// direct mutate the module instance in parser context.
impl<'a> Parser<'a> {
    /// Create Zhu IR parser.
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            module: Module::new(),
            function: Function::new(),
            block: Block(0),
        }
    }
    /// parse given source string.
    pub fn parse(&mut self) -> Module {
        self.parse_module()
    }
    /// Helper function to create function builder and
    /// switch to current block.
    fn create_builder(&mut self) -> FunctionBuilder {
        let mut builder = FunctionBuilder::new(&mut self.function);
        builder.switch_to_block(self.block);
        builder
    }
    /// Helper function to overwrite the value in function entity
    /// and function layout.
    /// - Usage: all `dst` value will overwrited by `src` value
    fn overwrite_value_when_def(&mut self, dst: Value, src: Value) {
        // overwrite entity
        let value_data = self.function.entities.values.remove(&dst).unwrap();
        let inst = match &value_data {
            ValueData::Inst { inst, .. } => inst.clone(),
            _ => unreachable!(),
        };
        self.function.entities.values.insert(src, value_data);
        // overwrite inst result
        self.function.entities.insts_result.insert(inst, src);
    }
    /// Helper function to rewrite the block in function entity and
    /// function layout.
    /// - Usage: all `dst` block will be rewrite by `src` block
    fn rewrite_block_when_def(&mut self, dst: Block, src: Block) {
        // rewrite entity
        let block_data = self.function.entities.blocks.remove(&dst).unwrap();
        self.function.entities.blocks.insert(src, block_data);
        // rewrite layout
        let block_node = self.function.layout.blocks.remove(&dst).unwrap();
        if let Some(pre_block) = block_node.prev {
            let pre_block_node = self.function.layout.blocks.get_mut(&pre_block).unwrap();
            pre_block_node.next = Some(src);
        }
        if let Some(next_block) = block_node.next {
            let next_block_node = self.function.layout.blocks.get_mut(&next_block).unwrap();
            next_block_node.prev = Some(src);
        }
        if self.function.layout.first_block == Some(dst) {
            self.function.layout.first_block = Some(src);
        }
        if self.function.layout.last_block == Some(dst) {
            self.function.layout.last_block = Some(src);
        }
        self.function.layout.blocks.insert(src, block_node);
    }
    /// Helper function to reset next context in function entity according to current max block and value index.
    fn reset_next_context_in_function_entities(&mut self) {
        if let Some(max) = self.function.entities.blocks.keys().map(|bb| bb.0).max() {
            self.function.entities.set_block_next_index(max + 1);
        };
        if let Some(max) = self.function.entities.values.keys().map(|value| value.0).max() {
            self.function.entities.set_value_next_index(max + 1);
        }
    }
    /// Helper to get size of value type, used when parse struct
    fn get_value_type_size(&mut self, value_type: &ValueType) -> u32 {
        match value_type {
            ValueType::U8 => 8,
            ValueType::U16 => 16,
            ValueType::U32 => 32,
            ValueType::U64 => 64,
            ValueType::I16 => 16,
            ValueType::I32 => 32,
            ValueType::I64 => 64,
            ValueType::F32 => 32,
            ValueType::F64 => 64,
            ValueType::Mem(mem_type) => {
                let mem_type_data = self.function.mem_type.get(&mem_type).unwrap();
                match mem_type_data {
                    MemTypeData::Array(array_type) => array_type.size,
                    MemTypeData::Struct(struct_type) => struct_type.size,
                    MemTypeData::Unknow => 64,
                }
            }
        }
    }
    /// Parse Module
    /// ```markdown
    /// <Module> := <DataStmts> | <Functions>
    /// ```
    fn parse_module(&mut self) -> Module {
        self.parse_data_statements();
        self.parse_functions();
        std::mem::replace(&mut self.module, Module::new())
    }
    /// Parse data statements
    /// ```markdown
    /// <DataStmts> := <DataStmts> <DataStmt>
    ///             := <DataStmt>
    /// ```
    fn parse_data_statements(&mut self) {
        while TokenKind::Identifier == self.lexer.get_token_kind() {
            self.parse_data_statement();
        }
    }
    /// Parse data statement
    /// ```markdown
    /// <Identifier> "=" "@" "data" "{""}"
    /// ```
    fn parse_data_statement(&mut self) {
        let id_str = parse_identifier!(self.lexer);
        expect_token!(self.lexer, TokenKind::Assign);
        expect_token!(self.lexer, TokenKind::At);
        expect_token!(self.lexer, TokenKind::DataKeyword);
        expect_token!(self.lexer, TokenKind::BracesLeft);
        expect_token!(self.lexer, TokenKind::BraceRight);
        let data_description = DataDescription::new();
        self.module.define_data(id_str, data_description);
    }
    /// Parse functions
    /// ```markdown
    /// <Functions> := <Functions> <Function>
    ///             := <Function>
    /// ```
    fn parse_functions(&mut self) {
        while match_tokens!(self.lexer, TokenKind::FuncKeyword) {
            self.parse_function();
        }
    }
    /// Parse function
    /// ```markdown
    /// <Function> := "func" <FunctionName> "(" <FunctionParams> ")" <FunctionBody>
    /// ```
    fn parse_function(&mut self) {
        expect_token!(self.lexer, TokenKind::FuncKeyword);
        let func_name = parse_identifier!(self.lexer);
        self.parse_function_params();
        self.parse_function_return_type();
        self.parse_function_body();
        self.reset_next_context_in_function_entities();
        self.module
            .define_function(func_name, std::mem::replace(&mut self.function, Function::new()));
    }
    /// Parse function param
    /// ```markdown
    /// <FunctionParams>    := <FunctionParams> "," <FunctionParam>
    ///                     := FunctionParam
    /// ```
    fn parse_function_params(&mut self) {
        expect_token!(self.lexer, TokenKind::ParanLeft);
        let mut is_start = true;
        while !match_tokens!(self.lexer, TokenKind::ParanRight, TokenKind::EOF) {
            if is_start {
                is_start = false;
            } else {
                expect_token!(self.lexer, TokenKind::Comma);
            }
            self.parse_reg();
            expect_token!(self.lexer, TokenKind::Colon);
            let ty = self.parse_value_type();
            self.function.def_func_param(ty);
        }
        expect_token!(self.lexer, TokenKind::ParanRight);
    }
    /// Parse function return type
    /// ```markdown
    /// <FunctionReturnType> := ":" <ValueType>
    /// ```
    fn parse_function_return_type(&mut self) {
        if match_tokens!(self.lexer, TokenKind::Colon) {
            self.lexer.next_token();
            let ty = self.parse_value_type();
            self.function.set_return_type(ty)
        }
    }
    /// Parse function body
    /// ```markdown
    /// <FunctionBody>  := "{" <GlobalStmts> <Blocks> "}"
    /// ```
    fn parse_function_body(&mut self) {
        expect_token!(self.lexer, TokenKind::BracesLeft);
        self.parse_structs();
        self.parse_global_statements();
        self.parse_blocks();
        expect_token!(self.lexer, TokenKind::BraceRight);
    }
    /// Parse struct defs
    fn parse_structs(&mut self) {
        while match_tokens!(self.lexer, TokenKind::StructKeyword) {
            self.parse_struct();
        }
    }
    /// Parse <StrcuDef>
    fn parse_struct(&mut self) {
        self.parse_struct_name();
        expect_token!(self.lexer, TokenKind::Assign);
        expect_token!(self.lexer, TokenKind::BracesLeft);
        let mut is_start = true;
        let mut offset = 0;
        let mut size = 0;
        let mut fields: Vec<StructTypeDataField> = Vec::new();
        while !match_tokens!(self.lexer, TokenKind::BraceRight, TokenKind::EOF) {
            if is_start {
                is_start = false;
            } else {
                expect_token!(self.lexer, TokenKind::Comma);
            }
            let ty = self.parse_value_type();
            size += self.get_value_type_size(&ty);
            fields.push(StructTypeDataField { offset, ty });
            offset += size
        }
        self.function
            .declar_mem_type(MemTypeData::Struct(StructTypeData { size: 0, fields }));
        expect_token!(self.lexer, TokenKind::BraceRight);
    }
    // Parse <StructName>
    fn parse_struct_name(&mut self) -> u8 {
        expect_token!(self.lexer, TokenKind::StructKeyword);
        expect_token!(self.lexer, TokenKind::Percent);
        self.parse_decimal_string::<u8>()
    }
    /// Parse global statements
    /// ```markdown
    /// <GlobalStmts>   := <GlobalStmts> <GlobalStmt>
    ///                 := <GlobalStmt>
    /// ```
    fn parse_global_statements(&mut self) {
        while match_tokens!(self.lexer, TokenKind::GReg) {
            self.parse_global_statement();
        }
    }
    /// Parse global statement
    /// - please reference to readme.
    fn parse_global_statement(&mut self) {
        self.parse_greg();
        expect_token!(self.lexer, TokenKind::Assign);
        expect_token!(self.lexer, TokenKind::At);
        expect_token!(self.lexer, TokenKind::GlobalKeyword);
        if TokenKind::SymbolKeyword == self.lexer.get_token_kind() {
            self.lexer.next_token();
            let sym_name = parse_identifier!(self.lexer);
            let module_id = self.module.get_module_id_by_symbol(sym_name).unwrap().clone();
            let external_name = match module_id {
                ModuleLevelId::Data(data_id) => ExternalName::UserDefName {
                    namespace: UserDefNamespace::Data,
                    value: data_id.0,
                },
                ModuleLevelId::Func(func_id) => ExternalName::UserDefName {
                    namespace: UserDefNamespace::Function,
                    value: func_id.0,
                },
            };
            self.function
                .declar_global_value(GlobalValueData::Symbol { name: external_name });
            return;
        }
        let ty = self.parse_value_type();
        let token_kind = self.lexer.get_token_kind();
        self.lexer.next_token();
        expect_token!(self.lexer, TokenKind::BracketLeft);
        let base = self.parse_greg();
        expect_token!(self.lexer, TokenKind::Comma);
        let offset = self.parse_offset();
        expect_token!(self.lexer, TokenKind::BracketRight);
        self.function.declar_global_value(match token_kind {
            TokenKind::LoadRegister => GlobalValueData::Load { base, offset, ty },
            TokenKind::AddI => GlobalValueData::AddI { base, offset, ty },
            _ => {
                panic!();
            }
        });
    }
    /// Parse blocks
    /// ```markdown
    /// <Blocks>    := (BlockLabel ":" "\n" <Instructions>)*
    /// ```
    fn parse_blocks(&mut self) {
        while match_tokens!(self.lexer, TokenKind::BlockLabel) {
            self.parse_block();
        }
    }
    /// Parse blocks
    /// ```markdown
    /// <Blocks> := (BlockLabel ":" "\n" <Instructions>)*
    /// ```
    fn parse_block(&mut self) {
        let rewrite_src = self.parse_block_label();
        let rewrite_dst = self.function.create_block();
        self.rewrite_block_when_def(rewrite_dst, rewrite_src);
        self.block = rewrite_src;
        expect_token!(self.lexer, TokenKind::Colon);
        self.parse_instructions();
    }
    /// Parse blocks
    /// ```markdown
    /// <BlockLabel> := "block" <DecimalString>
    /// ```
    fn parse_block_label(&mut self) -> Block {
        let bb_number = self.lexer.get_source_string()[5..]
            .parse::<u32>()
            .unwrap_or_else(|error| {
                panic!(
                    "[Error]: block label `{}` parse error. {:?}",
                    &self.lexer.get_source_string(),
                    error
                )
            });
        self.lexer.next_token();
        Block(bb_number)
    }
    /// Parse Instructions
    /// ```markdown
    /// <Instructions>  := <Instructions> "\n" <Instructions>
    ///                 := <Instruction>
    /// ```
    fn parse_instructions(&mut self) {
        while match_tokens!(
            self.lexer,
            TokenKind::Reg,
            TokenKind::Ret,
            TokenKind::Call,
            TokenKind::GlobalStore,
            TokenKind::StoreRegister,
            TokenKind::Jump,
            TokenKind::BrIf
        ) {
            self.parse_instruction();
        }
    }
    /// Parse instruction
    /// - please reference to instruction in readme.
    fn parse_instruction(&mut self) {
        match self.lexer.get_token_kind() {
            TokenKind::Ret => {
                self.lexer.next_token();
                let reg = if TokenKind::Reg == self.lexer.get_token_kind() {
                    Some(self.parse_reg())
                } else {
                    None
                };
                self.create_builder().ret_inst(reg);
            }
            TokenKind::Jump => {
                self.lexer.next_token();
                let bb = self.parse_block_label();
                self.create_builder().jump_inst(bb);
            }
            TokenKind::BrIf => {
                self.lexer.next_token();
                let test = self.parse_reg();
                let conseq = self.parse_block_label();
                let alter = self.parse_block_label();
                self.create_builder().brif_inst(test, conseq, alter);
            }
            TokenKind::GlobalStore => {
                self.lexer.next_token();
                let src = self.parse_reg();
                let (base, offset) = self.parse_global_address();
                self.create_builder().global_store_inst(base, offset, src);
            }
            TokenKind::StoreRegister => {
                self.lexer.next_token();
                let src = self.parse_reg();
                let (base, offset) = self.parse_address();
                self.create_builder().store_inst(base, offset, src);
            }
            TokenKind::Call => {
                self.parse_right_hand_side_of_call_inst();
            }
            TokenKind::Reg => {
                let rewrite_src = self.parse_reg();
                expect_token!(self.lexer, TokenKind::Assign);
                let rewrite_dst = match self.lexer.get_token_kind() {
                    // Const
                    TokenKind::Iconst | TokenKind::Uconst | TokenKind::Fconst => {
                        let opcode = map_token_to_opcode(self.lexer.get_token_kind());
                        self.lexer.next_token();
                        let value_type = self.parse_value_type();
                        let bytes = self.parse_const_data();
                        self.create_builder().build_const_inst(opcode, bytes, value_type)
                    }
                    // Unary
                    TokenKind::Mov | TokenKind::Neg => {
                        let opcode = map_token_to_opcode(self.lexer.get_token_kind());
                        self.lexer.next_token();
                        let arg = self.parse_reg();
                        self.create_builder().build_unary_inst(opcode, arg)
                    }
                    // Binary
                    TokenKind::Add
                    | TokenKind::Sub
                    | TokenKind::Mul
                    | TokenKind::Divide
                    | TokenKind::Reminder
                    | TokenKind::FAdd
                    | TokenKind::FSub
                    | TokenKind::FMul
                    | TokenKind::FDivide
                    | TokenKind::FReminder
                    | TokenKind::BitwiseOR
                    | TokenKind::BitwiseAnd
                    | TokenKind::ShiftRight
                    | TokenKind::ShiftLeft => {
                        let opcode = map_token_to_opcode(self.lexer.get_token_kind());
                        self.lexer.next_token();
                        let args = [self.parse_reg(), self.parse_reg()];
                        self.create_builder().build_binary_inst(opcode, args)
                    }
                    // Binary Immi
                    TokenKind::AddI | TokenKind::SubI | TokenKind::DivideI | TokenKind::MulI | TokenKind::ReminderI => {
                        let opcode = map_token_to_opcode(self.lexer.get_token_kind());
                        self.lexer.next_token();
                        let arg = self.parse_reg();
                        let value_type = self.function.value_type(arg).clone();
                        let immediate = self.parse_immediate_by_value_type(value_type);
                        self.create_builder().build_binary_imm_inst(opcode, arg, immediate)
                    }
                    // Convert
                    TokenKind::ToU8
                    | TokenKind::ToU16
                    | TokenKind::ToU32
                    | TokenKind::ToU64
                    | TokenKind::ToI16
                    | TokenKind::ToI32
                    | TokenKind::ToI64
                    | TokenKind::ToF32
                    | TokenKind::ToF64
                    | TokenKind::ToAddress => {
                        let opcode = map_token_to_opcode(self.lexer.get_token_kind());
                        self.lexer.next_token();
                        let arg = self.parse_reg();
                        self.create_builder().build_convert_inst(opcode, arg)
                    }
                    // call with reg
                    TokenKind::Call => self.parse_right_hand_side_of_call_inst().unwrap(),
                    // Cmp
                    TokenKind::Icmp => {
                        self.lexer.next_token();
                        let cmp = map_token_to_cmp(self.lexer.get_token_kind());
                        self.lexer.next_token();
                        let args = [self.parse_reg(), self.parse_reg()];
                        self.create_builder().icmp_inst(cmp, args)
                    }
                    TokenKind::Fcmp => {
                        self.lexer.next_token();
                        let cmp = map_token_to_cmp(self.lexer.get_token_kind());
                        self.lexer.next_token();
                        let args = [self.parse_reg(), self.parse_reg()];
                        self.create_builder().fcmp_inst(cmp, args)
                    }
                    // Memory relate
                    TokenKind::LoadRegister => {
                        self.lexer.next_token();
                        let ty = self.parse_value_type();
                        let (base, offset) = self.parse_address();
                        self.create_builder().load_inst(base, offset, ty)
                    }
                    TokenKind::GlobalLoad => {
                        self.lexer.next_token();
                        let ty = self.parse_value_type();
                        let (base, offset) = self.parse_global_address();
                        self.create_builder().global_load_inst(base, offset, ty)
                    }
                    TokenKind::StackAlloc => {
                        self.lexer.next_token();
                        let ty = self.parse_value_type();
                        expect_token!(self.lexer, TokenKind::Comma);
                        expect_token!(self.lexer, TokenKind::SizeKeyword);
                        let size = self.parse_immediate_by_value_type(ValueType::U32);
                        expect_token!(self.lexer, TokenKind::Comma);
                        expect_token!(self.lexer, TokenKind::AlignKeyword);
                        let align = self.parse_immediate_by_value_type(ValueType::U8);
                        self.create_builder().stack_alloc_inst(size, align, ty)
                    }
                    // Phi
                    TokenKind::Phi => {
                        self.lexer.next_token();
                        let args = self.parse_phi_arguments();
                        self.create_builder().phi_inst(args)
                    }
                    _ => unexpect_token!(self.lexer),
                };
                self.overwrite_value_when_def(rewrite_dst, rewrite_src);
            }
            _ => unreachable!(),
        }
    }
    /// Parse right hand side of call instruction
    /// ```markdown
    /// "call" "func" <Identifier> "(" <FunctionArguments> ")"
    /// ```
    fn parse_right_hand_side_of_call_inst(&mut self) -> Option<Value> {
        self.lexer.next_token();
        expect_token!(self.lexer, TokenKind::FuncKeyword);
        let func_name = parse_identifier!(self.lexer);
        let func_ref = match self.module.get_module_id_by_symbol(func_name).unwrap() {
            ModuleLevelId::Func(func_id) => {
                let name = ExternalName::UserDefName {
                    namespace: UserDefNamespace::Function,
                    value: func_id.0,
                };
                let sig = self.module.functions.get(func_id).unwrap().signature.clone();
                self.function
                    .declar_external_function(ExternalFunctionData { name, sig })
            }
            _ => unreachable!(),
        };
        let params = self.parse_call_arguments();
        self.create_builder().call_inst(params, func_ref)
    }
    /// Parse function arguments
    /// ```markdown
    /// <FunctionArguments> := <FunctionAreguments> "," <FunctionArgument>
    ///                     := <FunctionArgument>
    /// ```
    fn parse_call_arguments(&mut self) -> Vec<Value> {
        expect_token!(self.lexer, TokenKind::ParanLeft);
        let mut is_start = true;
        let mut params = Vec::new();
        while !match_tokens!(self.lexer, TokenKind::ParanRight, TokenKind::EOF) {
            if is_start {
                is_start = false;
            } else {
                expect_token!(self.lexer, TokenKind::Comma);
            }
            params.push(self.parse_reg());
        }
        expect_token!(self.lexer, TokenKind::ParanRight);
        params
    }
    /// Parse phi arguments
    /// ```markdown
    /// <PhiArguments>  := <PhiArguments> "," <PhiArgument>
    ///                 := <PhiArgument>
    /// ```
    fn parse_phi_arguments(&mut self) -> Vec<(Block, Value)> {
        expect_token!(self.lexer, TokenKind::BracketLeft);
        let mut is_start = true;
        let mut params = Vec::new();
        while !match_tokens!(self.lexer, TokenKind::BracketRight, TokenKind::EOF) {
            if is_start {
                is_start = false;
            } else {
                expect_token!(self.lexer, TokenKind::Comma);
            }
            params.push((self.parse_block_label(), self.parse_reg()));
        }
        expect_token!(self.lexer, TokenKind::BracketRight);
        params
    }
    /// Parse Address
    /// ```markdown
    /// <Address> := "[" <VReg> "," <Offset> "]"
    /// ```
    fn parse_address(&mut self) -> (Value, Offset) {
        expect_token!(self.lexer, TokenKind::BracketLeft);
        let base = self.parse_reg();
        expect_token!(self.lexer, TokenKind::Comma);
        let offset = self.parse_offset();
        expect_token!(self.lexer, TokenKind::BracketRight);
        (base, offset)
    }
    /// Parse Address
    /// ```markdown
    /// <GlobalAddress> := "[" <GReg> "," <Offset> "]"
    /// ```
    fn parse_global_address(&mut self) -> (GlobalValue, Offset) {
        expect_token!(self.lexer, TokenKind::BracketLeft);
        let base = self.parse_greg();
        expect_token!(self.lexer, TokenKind::Comma);
        let offset = self.parse_offset();
        expect_token!(self.lexer, TokenKind::BracketRight);
        (base, offset)
    }
    /// Parse Value type
    /// - please reference to readme
    fn parse_value_type(&mut self) -> ValueType {
        match self.lexer.get_token_kind() {
            TokenKind::U8Keyword => {
                self.lexer.next_token();
                ValueType::U8
            }
            TokenKind::U16Keyword => {
                self.lexer.next_token();
                ValueType::U16
            }
            TokenKind::I16Keyword => {
                self.lexer.next_token();
                ValueType::I16
            }
            TokenKind::I32Keyword => {
                self.lexer.next_token();
                ValueType::I32
            }
            TokenKind::I64Keyword => {
                self.lexer.next_token();
                ValueType::I64
            }
            TokenKind::F32Keyword => {
                self.lexer.next_token();
                ValueType::F32
            }
            TokenKind::F64Keyword => {
                self.lexer.next_token();
                ValueType::F64
            }
            TokenKind::MemKeyword => {
                self.lexer.next_token();
                let mem_type = self.function.declar_mem_type(MemTypeData::Unknow);
                ValueType::Mem(mem_type)
            }
            TokenKind::StructKeyword => {
                let mem_type_num = self.parse_struct_name();
                let mem_type = MemType(mem_type_num as u32);
                ValueType::Mem(mem_type)
            }
            _ => {
                unexpect_token!(self.lexer)
            }
        }
    }
    /// Parse reg
    /// ```markdown
    /// <VReg>  := "reg"(no skipable char)<DecimalString>
    /// ```
    fn parse_reg(&mut self) -> Value {
        if let TokenKind::Reg = self.lexer.get_token_kind() {
            let reg_number = self.lexer.get_source_string()[3..].parse::<u32>().unwrap();
            self.lexer.next_token();
            return Value(reg_number as u32);
        } else {
            unexpect_token!(self.lexer)
        }
    }
    /// Parse greg
    /// ```markdown
    /// <GReg>  := "greg"(no skipable char)<DecimalString>
    /// ```
    fn parse_greg(&mut self) -> GlobalValue {
        if let TokenKind::GReg = self.lexer.get_token_kind() {
            let reg_number = self.lexer.get_source_string()[4..].parse::<u32>().unwrap();
            self.lexer.next_token();
            return GlobalValue(reg_number as u32);
        } else {
            unexpect_token!(self.lexer)
        }
    }
    /// Parse offset
    /// - this function will cover type to i32 offset.
    /// ```markdown
    /// <Offset> := <DecimalString> | <HexString>
    /// ```
    fn parse_offset(&mut self) -> Offset {
        match self.lexer.get_token_kind() {
            TokenKind::DecimalString => Offset(self.parse_decimal_string::<i32>()),
            TokenKind::HexString => Offset(self.parse_hex_string::<i32>()),
            _ => unexpect_token!(self.lexer),
        }
    }
    /// Parse Const data
    /// ```markdown
    ///
    /// ```
    fn parse_const_data(&mut self) -> Vec<u8> {
        expect_token!(self.lexer, TokenKind::BracketLeft);
        let mut bytes = Vec::<u8>::new();
        while !match_tokens!(self.lexer, TokenKind::BracketRight | TokenKind::EOF) {
            bytes.push(self.parse_hex_string::<u8>());
        }
        expect_token!(self.lexer, TokenKind::BracketRight);
        bytes
    }
    /// Parse immediate
    /// - parse immediate only used when parse binary immediate, so we can resolve type by value type.
    /// ```markdown
    /// <Immediate> := <DecimalString> | <HexString>
    /// ```
    fn parse_immediate_by_value_type(&mut self, value_type: ValueType) -> Immediate {
        match value_type {
            ValueType::U8 => Immediate::U8(parse_immediate_helper_marco!(self, u8)),
            ValueType::U16 => Immediate::U16(parse_immediate_helper_marco!(self, u16)),
            ValueType::U32 => Immediate::U32(parse_immediate_helper_marco!(self, u32)),
            ValueType::U64 => Immediate::U64(parse_immediate_helper_marco!(self, u64)),
            ValueType::I16 => Immediate::I16(parse_immediate_helper_marco!(self, i16)),
            ValueType::I32 => Immediate::I32(parse_immediate_helper_marco!(self, i32)),
            ValueType::I64 => Immediate::I64(parse_immediate_helper_marco!(self, i64)),
            ValueType::F32 => Immediate::F32(parse_immediate_helper_marco!(self, f32)),
            ValueType::F64 => Immediate::F64(parse_immediate_helper_marco!(self, f64)),
            _ => panic!("[Error]: value type {:?} can not use as immediate.", value_type),
        }
    }
    /// Parse decimal string with given type
    /// - usually used for other function to parse decimal string with given type
    fn parse_decimal_string<T>(&mut self) -> T
    where
        T: FromStr,
    {
        if let TokenKind::DecimalString = self.lexer.get_token_kind() {
            let value = self.lexer.get_source_string().parse::<T>();
            self.lexer.next_token();
            return value.unwrap_or_else(|_| panic!("[Error]: parse decimal string error"));
        } else {
            unexpect_token!(self.lexer)
        }
    }
    /// Parse hex string with given type
    /// - usually used for other function to parse hex string with given type
    fn parse_hex_string<T>(&mut self) -> T
    where
        T: FromStr,
    {
        if let TokenKind::HexString = self.lexer.get_token_kind() {
            let value = self.lexer.get_source_string()[2..].parse::<T>();
            self.lexer.next_token();
            return value.unwrap_or_else(|_| panic!("[Error]: parse hex string error."));
        } else {
            unexpect_token!(self.lexer)
        }
    }
}
