use std::str::CharIndices;

use crate::frontend::token::{Token, TokenKind};

pub struct Lexer<'a> {
    source: &'a str,
    chars: CharIndices<'a>,
    start_pos: usize,
    end_pos: usize,
    cur_pos: usize,
    cur_char: Option<char>,
    cur_token: Token,
}
/// ### Marco to match single char token
/// eat char and finish it.
macro_rules! finish_token_and_eat {
    ($lexer: expr,$kind: expr) => {{
        $lexer.eat_char();
        $lexer.finish_token($kind)
    }};
}

// TODO: for better performance, using byte to read to string.
impl<'a> Lexer<'a> {
    /// Create a empty lexer, cur char is None, will always return EOF
    /// token if we call `next_token` method.
    fn empty_lexer(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices(),
            start_pos: 0,
            end_pos: 0,
            cur_pos: 0,
            cur_char: None,
            cur_token: Token {
                kind: TokenKind::EOF,
                start: 0,
                end: 0,
            },
        }
    }
    /// Create a new lexer, accpet source is a empty string
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::empty_lexer(source);
        if let Some(tuple) = lexer.chars.next() {
            lexer.cur_pos = tuple.0;
            lexer.cur_char = Some(tuple.1);
            lexer.lex();
            lexer
        } else {
            lexer
        }
    }
    /// Move to next token, it will keep product TokenKind::EOF token with same
    /// info when read end of file.
    pub fn next_token(&mut self) {
        self.lex();
    }
    /// Get current token info, it will clone entire token, including span.
    pub fn get_token(&self) -> Token {
        self.cur_token.clone()
    }
    /// Get current token kind, only get the kind, reduce the space of clone.
    pub fn get_token_kind(&self) -> TokenKind {
        self.cur_token.kind.clone()
    }
    /// Get current token source value, borrow a immutable reference of original
    /// source string
    pub fn get_source_string(&self) -> &'a str {
        &self.source[self.cur_token.start..self.cur_token.end]
    }
    /// Get current token start pos.
    pub fn get_start_pos(&self) -> usize {
        self.start_pos
    }
    /// Get current token end pos.
    pub fn get_end_pos(&self) -> usize {
        self.end_pos
    }
    /// ### Internal method to get char
    /// Get current char, if reach eof, it will return None.
    fn get_char(&self) -> Option<char> {
        self.cur_char
    }
    /// ### Internal method to eat char
    /// If reach EOF, this function will not mutate the lexer state.
    fn eat_char(&mut self) {
        if let Some((pos, ch)) = self.chars.next() {
            self.cur_pos = pos;
            self.cur_char = Some(ch)
        } else {
            self.cur_char = None;
        }
    }
    /// ### Internal method to record start position of token
    fn start_token(&mut self) {
        self.start_pos = self.cur_pos;
    }
    /// ### Internal method to record end position of token
    fn finish_token(&mut self, kind: TokenKind) {
        self.end_pos = self.cur_pos;
        self.cur_token = Token {
            kind,
            start: self.start_pos,
            end: self.end_pos,
        };
    }
    /// ### Main state mahcine of tokenize
    fn lex(&mut self) {
        self.skip_space_and_change_line();
        self.start_token();
        match self.get_char() {
            None => finish_token_and_eat!(self, TokenKind::EOF),
            Some(ch) => match ch {
                '{' => finish_token_and_eat!(self, TokenKind::BracesLeft),
                '}' => finish_token_and_eat!(self, TokenKind::BraceRight),
                '[' => finish_token_and_eat!(self, TokenKind::BracketLeft),
                ']' => finish_token_and_eat!(self, TokenKind::BracketRight),
                '(' => finish_token_and_eat!(self, TokenKind::ParanLeft),
                ')' => finish_token_and_eat!(self, TokenKind::ParanRight),
                '=' => finish_token_and_eat!(self, TokenKind::Assign),
                ',' => finish_token_and_eat!(self, TokenKind::Comma),
                '@' => finish_token_and_eat!(self, TokenKind::At),
                ':' => finish_token_and_eat!(self, TokenKind::Colon),
                '%' => finish_token_and_eat!(self, TokenKind::Percent),
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    println!("{:?}", &self.source[self.cur_pos..]);
                    if self.source[self.cur_pos..].starts_with("0x") {
                        // eat "0x"
                        self.eat_char();
                        self.eat_char();
                        self.read_hex();
                        self.finish_token(TokenKind::HexString);
                    } else {
                        self.read_decimal();
                        self.finish_token(TokenKind::DecimalString);
                    }
                }
                _ => {
                    let tk = self.read_keyword();
                    self.finish_token(tk);
                }
            },
        }
    }
    /// ### Internal method to skip ignoreable char
    /// eat '\n', '\t', ' ' util reach other char.
    fn skip_space_and_change_line(&mut self) {
        loop {
            match self.get_char() {
                None => break,
                Some(ch) => match ch {
                    '\n' | '\t' | ' ' => self.eat_char(),
                    _ => break,
                },
            }
        }
    }
    /// ### Internal method to skip ignoreable char
    /// eat '\n', '\t', ' ' util reach other char.
    fn read_word(&mut self) -> &'a str {
        let start_pos = self.cur_pos;
        loop {
            match self.get_char() {
                None => break,
                Some(ch) => match ch {
                    '\n' | '\t' | ' ' => break,
                    '{' | '}' | '[' | ']' | '(' | ')' | '=' | ',' | '@' | ':' | '%' => break,
                    _ => self.eat_char(),
                },
            }
        }
        &self.source[start_pos..self.cur_pos]
    }
    /// ### Internal method to read decimal string
    /// eat digital char util reach other char, please note
    /// that this method also accept string start with 0.
    fn read_decimal(&mut self) -> &'a str {
        let start_pos = self.cur_pos;
        loop {
            match self.get_char() {
                None => break,
                Some(ch) => match ch {
                    '0'..'9' => self.eat_char(),
                    _ => break,
                },
            }
        }
        &self.source[start_pos..self.cur_pos]
    }
    /// ### Internal method to read hex strin
    /// eat digital char util reach other char, please note
    /// that need to check is start with `0x` before
    /// call this method
    fn read_hex(&mut self) -> &'a str {
        let start_pos = self.cur_pos;
        loop {
            match self.get_char() {
                None => break,
                Some(ch) => match ch {
                    '0'..'9' | 'A'..'F' => self.eat_char(),
                    _ => break,
                },
            }
        }
        &self.source[start_pos..self.cur_pos]
    }
    /// ### Internal method to match keyword and symbol
    fn read_keyword(&mut self) -> TokenKind {
        let word = self.read_word();
        match word {
            "eq" => TokenKind::Eq,
            "noteq" => TokenKind::NotEq,
            "gt" => TokenKind::Gt,
            "gteq" => TokenKind::Gteq,
            "lt" => TokenKind::Lt,
            "lteq" => TokenKind::LtEq,
            "uconst" => TokenKind::Uconst,
            "iconst" => TokenKind::Iconst,
            "fconst" => TokenKind::Fconst,
            "add" => TokenKind::Add,
            "addi" => TokenKind::AddI,
            "sub" => TokenKind::Sub,
            "subi" => TokenKind::SubI,
            "mul" => TokenKind::Mul,
            "muli" => TokenKind::MulI,
            "divide" => TokenKind::Divide,
            "dividei" => TokenKind::DivideI,
            "reminder" => TokenKind::Reminder,
            "reminderi" => TokenKind::ReminderI,
            "fadd" => TokenKind::FAdd,
            "fsub" => TokenKind::FSub,
            "fmul" => TokenKind::FMul,
            "fdivide" => TokenKind::FDivide,
            "freminder" => TokenKind::FReminder,
            "bnot" => TokenKind::BitwiseNot,
            "bor" => TokenKind::BitwiseOR,
            "band" => TokenKind::BitwiseAnd,
            "shl" => TokenKind::ShiftLeft,
            "shr" => TokenKind::ShiftRight,
            "mov" => TokenKind::Mov,
            "neg" => TokenKind::Neg,
            "icmp" => TokenKind::Icmp,
            "fcmp" => TokenKind::Fcmp,
            "call" => TokenKind::Call,
            "ret" => TokenKind::Ret,
            "to.u8" => TokenKind::ToU8,
            "to.u16" => TokenKind::ToU16,
            "to.u32" => TokenKind::ToU32,
            "to.u64" => TokenKind::ToU64,
            "to.i16" => TokenKind::ToI16,
            "to.i32" => TokenKind::ToI32,
            "to.i64" => TokenKind::ToI64,
            "to.f32" => TokenKind::ToF32,
            "to.f64" => TokenKind::ToF64,
            "to.addr" => TokenKind::ToAddress,
            "stackalloc" => TokenKind::StackAlloc,
            "stackaddr" => TokenKind::StackAddr,
            "load" => TokenKind::LoadRegister,
            "store" => TokenKind::StoreRegister,
            "gload" => TokenKind::GlobalLoad,
            "gstore" => TokenKind::GlobalStore,
            "brif" => TokenKind::BrIf,
            "jump" => TokenKind::Jump,
            "phi" => TokenKind::Phi,
            "symbol" => TokenKind::SymbolKeyword,
            "global" => TokenKind::GlobalKeyword,
            "struct" => TokenKind::StructKeyword,
            "mem" => TokenKind::MemKeyword,
            "func" => TokenKind::FuncKeyword,
            "data" => TokenKind::DataKeyword,
            "size" => TokenKind::SizeKeyword,
            "align" => TokenKind::AlignKeyword,
            "u8" => TokenKind::U8Keyword,
            "u16" => TokenKind::U16Keyword,
            "i16" => TokenKind::I16Keyword,
            "i32" => TokenKind::I32Keyword,
            "i64" => TokenKind::I64Keyword,
            "f32" => TokenKind::F32Keyword,
            "f64" => TokenKind::F64Keyword,
            _ => {
                if word.starts_with("reg") {
                    TokenKind::Reg
                } else if word.starts_with("greg") {
                    TokenKind::GReg
                } else if word.starts_with("block") {
                    TokenKind::BlockLabel
                } else {
                    TokenKind::Identifier
                }
            }
        }
    }
}
