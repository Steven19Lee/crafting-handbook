# Zhu IR

IR for educational purpose, just for fun.

## Language Spec

```
// Top Level
<Module>       := <DataStmts> | <Functions>

// struct def
<StructDefs>    := <StructDefs> <StructDef>
                := <StructDef>

<StructDef>     := <StructName> "=" "{" <StructDefField> "}"
<StructName>    := "struct" "." <DecimalString>
<StructDefFields>       := <StructDefFields> "," <StructDefField> 
                        := <StructDefField>
<StructDefField>  := <ValueType>

// Data statement 
<DataStmts>     := <DataStmts> <DataStmt>
                := <DataStmt>
<DataStmt>      := <Identifier> "=" "@" "data" "{""}"

// Function
<Functions>     := <Functions> <Function>
                := <Function>
<Function>      := "func" <FunctionName> "(" <FunctionParams> ")" <FunctionReturnType>? <FunctionBody>
<FunctionReturnType>    := ":" <ValueType>
<FunctionName>  := <Identifier>
<FunctionParams>:= <FunctionParams> "," <FunctionParam>
                := FunctionParam
<FunctionParam> := <VReg> ":" <ValueType>
<FunctionBody>  := "{" <StructDefs> <GlobalStmts> <Blocks> "}"

<GlobalStmts>   := <GlobalStmts> <GlobalStmt>
                := <GlobalStmt>
<GlobalStmt>    := <GReg> "=" <GlobalSymbolDeclar>
                := <GReg> "=" <GlobalLoadDelcar>
                := <GReg> "=" <GlobalConstAddDeclar>
<GlobalSymbolDeclar>    := "@" "global" "symbol" <Identifier>
<GlobalLoadDeclar>      := "@" "global" <ValueType>, "load" "[" <GReg> "," <DecimalString> "]"
<GlobalConstAddDeclar>  := "@" "global" <ValueType>, "addi" "[" <GReg> "," <DecimalString> "]"

<Blocks>        := (BlockLabel ":" "\n" <Instructions>)*
<BlockLabel>    := "block" <DecimalString>
<Instructions>  := <Instructions> "\n" <Instructions>
                := <Instruction>

<Instruction> 
        := <VReg> "=" "uconst" <ValueType> <ConstData>
        := <VReg> "=" "iconst" <ValueType> <ConstData>
        := <VReg> "=" "fconst" <ValueType> <ConstData>
        := <VReg> "=" "add" <VReg> <VReg> 
        := <VReg> "=" "addi" <VReg> <Immediate>
        := <VReg> "=" "sub" <VReg> <VReg> 
        := <VReg> "=" "subi" <VReg> <Immediate>
        := <VReg> "=" "mul" <VReg> <VReg> 
        := <VReg> "=" "muli" <VReg> <Immediate>
        := <VReg> "=" "divide" <VReg> <VReg> 
        := <VReg> "=" "dividei" <VReg> <Immediate>
        := <VReg> "=" "fadd" <VReg> <VReg> 
        := <VReg> "=" "fsub" <VReg> <VReg> 
        := <VReg> "=" "fmul" <VReg> <VReg> 
        := <VReg> "=" "fdivide" <VReg> <VReg> 
        := <VReg> "=" "freminder" <VReg> <VReg> 
        := <VReg> "=" "bnot" <VReg>
        := <VReg> "=" "bor" <VReg> <VReg> 
        := <VReg> "=" "band" <VReg> <VReg> 
        := <VReg> "=" "shl" <VReg> <VReg> 
        := <VReg> "=" "shr" <VReg> <VReg> 
        := <VReg> "=" "mov" <VReg>
        := <VReg> "=" "neg" <VReg>
        := <VReg> "=" "icmp" <CmpFlag> <VReg> <VReg>
        := <VReg> "=" "fcmp" <CmpFlag> <VReg> <VReg>
        := (<VReg> "=")? "call" "func" <Identifier> "(" <FunctionArguments> ")"
        := "ret" <VReg>?
        := <VReg> "=" "to.u8"  <VReg>
        := <VReg> "=" "to.u16" <VReg>
        := <VReg> "=" "to.u32" <VReg>
        := <VReg> "=" "to.u64" <VReg>
        := <VReg> "=" "to.i16" <VReg>
        := <VReg> "=" "to.i32" <VReg>
        := <VReg> "=" "to.i64" <VReg>
        := <VReg> "=" "to.f32" <VReg>
        := <VReg> "=" "to.f64" <VReg>
        := <VReg> "=" "to.address" <VReg>
        := <VReg> "=" "stackalloc" <ValueType> "," "size" <Immdiate> , "align" <Immdiate>
        := <VReg> "=" "stackaddr" <VReg>
        := <VReg> "=" "load" <ValueType> <Address>
        := "store" <VReg> <Address>
        := <VReg> "=" "gload" <ValueType> <GlobalAddress>
        := "gstore" <VReg> <GlobalAddress>
        := "brif" <VReg> <BlockLabel> <BlockLabel> 
        := "jump" <BlockLabel>
        := <VReg> "=" "phi" "[" <PhiArguments> "]"

<FunctionArguments>     := <FunctionAreguments> "," <FunctionArgument>
                        := <FunctionArgument>
<FunctionArgument>      := <VReg>
<PhiArguments>          := <PhiArguments> "," <PhiArgument>
                        := <PhiArgument>
<PhiArgument>           := <BlockLabel> <VReg>
<Address>               := "[" <VReg> "," <Offset> "]"
<GlobalAddress>         := "[" <GReg> "," <Offset> "]"
// Token
<Token>         := <RegTk>
                := <TyTk>
                := <LiterakTk>
                := <PTk>
                := <KeywordTk>
                := <Identifier>

/// 1. Reg
<VReg>          := "reg"(no skipable char)<DecimalString>
<GReg>          := "greg"(no skipable char)<DecimalString>

/// 2. Type
<ValueType>     := "u8"
                := "u16"
                := "i16"
                := "i32"
                := "i64"
                := "f32"
                := "f64"
                := "ptr"
                := "struct" "." <Identifier>
                := <ArrayVty>
<ArrayVty>      := "[" <DecimalString> "*" "ValueTyep"(not array type) "]"

/// 3. Literal
<Constant>      := "[" HexPairs "]"
<Immediate>     := <DecimalString> | <HexString>
<Offset>        := <DecimalString> | <HexString>
<ConstData>     := "[" <HexPairs> "]"
<HexPairs>      := <HexPairs> <HexPair>
                := <HexPair>
<HexPair>       := "0x" <HexChar> <HexChar>
<DecimalString> := ("+" | "-")? ("0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9")*
<HexLiteral>    := ("+" | "-")? (<HexChar>)*
<HexChar>       := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "A" | "B" | "C" | "D" | "E" | "F"

/// 4. Punctuator
<Punctuator> := "=" | "(" | ")" | "[" | "]" | "{" |"}" | "@" | ":"
/// 5. Keyword
<Keyword>
        := <CmpFlag>
        := <OpcodeKeyword>
        := <SymbolKeyword>
        := <GlobalKeyword>
        := <FuncKeyword>
        := <DataKeyword>
        := <CallKeyword>
        := <SizeKeyword>
        := <AlignKeyword>

<CmpFlag> := "eq"
          := "noteq"
          := "gt"
          := "gteq"
          := "lt"
          := "lteq"        

<OpcodeKeyword>
        := "uconst"
        := "iconst"
        := "f32const"
        := "f64const"
        := "add"
        := "addi"
        := "sub" 
        := "subi"
        := "mul" 
        := "muli"
        := "divide" 
        := "dividei"
        := "reminder"
        := "reminderi"
        := "fadd" 
        := "fsub" 
        := "fmul" 
        := "fdivide" 
        := "freminder" 
        := "bnot" 
        := "bor" 
        := "band" 
        := "shl" 
        := "shr" 
        := "mov"
        := "neg"
        := "icmp" 
        := "fcmp"
        := "call"
        := "ret"
        := "to.u8"
        := "to.u16"
        := "to.u32"
        := "to.u64"
        := "to.i16"
        := "to.i32"
        := "to.i64"
        := "to.f32"
        := "to.f64"
        := "to.addr"
        := "stackalloc"
        := "stackaddr"
        := "load"
        := "store"
        := "gload"
        := "gstore"
        := "brif"
        := "jump"
        := "phi"

<SymbolKeyword> := "symbol"
<GlobalKeyword> := "global"
<FuncKeyword>   := "func"
<CallKeyword>   := "call"
<SizeKeyword>   := "size"
<AlignKeyword>  := "align"
```

## Test Strcuture


### How to create baseline test cases

