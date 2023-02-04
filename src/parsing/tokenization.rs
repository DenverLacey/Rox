use std::collections::VecDeque;
use std::fmt::Display;
use std::str::Chars;

use enum_tags::*;
use miette::SourceSpan;

use crate::interp::LoadedFile;
use crate::util::errors::SourceError;
use crate::util::{
    errors::Result,
    iter::{VeryPeekable, VeryPeekableIterExt},
};

#[derive(Clone, Copy, Debug)]
pub struct CodeLocation {
    pub loaded_file_idx: usize,
    pub span: SourceSpan,
}

impl CodeLocation {
    pub fn new(loaded_file_idx: usize, span: impl Into<SourceSpan>) -> Self {
        Self {
            loaded_file_idx,
            span: span.into(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub loc: CodeLocation,
    pub info: TokenInfo,
}

impl Token {
    fn new(loc: CodeLocation, info: TokenInfo) -> Self {
        Self { loc, info }
    }

    pub fn is_end(&self) -> bool {
        self.info.tag() == TokenInfoTag::End
    }
}

#[derive(Clone, Debug, Tag)]
pub enum TokenInfo {
    End,

    // Literals
    Ident(String), // @IMPROVE: This could probably be a &str with some lifetime
    Bool(bool),
    Char(char),
    Int(i64),
    Float(f64),
    String(String), // @IMPOVE: ^^

    // Delimeters
    Newline,
    Comma,
    Colon,
    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
    SqrBracketOpen,
    SqrBracketClose,

    // Operators
    Plus,
    Dash,
    Star,
    Slash,
    Percent,
    Bang,
    Equal,
    Ampersand,
    Dot,
    DotDot,

    // Keywords
    Import,
    Let,
    Mut,
    Fn,
    As,
    If,
    Else,
    Struct,
    Return,

    XXXPrint,
}

impl TokenInfo {
    pub const fn precedence(&self) -> TokenPrecedence {
        match *self {
            TokenInfo::End => TokenPrecedence::None,
            TokenInfo::Ident(_) => TokenPrecedence::None,
            TokenInfo::Bool(_) => TokenPrecedence::None,
            TokenInfo::Char(_) => TokenPrecedence::None,
            TokenInfo::Int(_) => TokenPrecedence::None,
            TokenInfo::Float(_) => TokenPrecedence::None,
            TokenInfo::String(_) => TokenPrecedence::None,
            TokenInfo::Newline => TokenPrecedence::None,
            TokenInfo::Comma => TokenPrecedence::None,
            TokenInfo::Colon => TokenPrecedence::Colon,
            TokenInfo::ParenOpen => TokenPrecedence::Call,
            TokenInfo::ParenClose => TokenPrecedence::None,
            TokenInfo::CurlyOpen => TokenPrecedence::None,
            TokenInfo::CurlyClose => TokenPrecedence::None,
            TokenInfo::SqrBracketOpen => TokenPrecedence::Call,
            TokenInfo::SqrBracketClose => TokenPrecedence::None,
            TokenInfo::Plus => TokenPrecedence::Term,
            TokenInfo::Dash => TokenPrecedence::Term,
            TokenInfo::Star => TokenPrecedence::Factor,
            TokenInfo::Slash => TokenPrecedence::Factor,
            TokenInfo::Percent => TokenPrecedence::Factor,
            TokenInfo::Bang => TokenPrecedence::Unary,
            TokenInfo::Equal => TokenPrecedence::None,
            TokenInfo::Ampersand => TokenPrecedence::BitAnd,
            TokenInfo::Dot => TokenPrecedence::Call,
            TokenInfo::DotDot => TokenPrecedence::Range,
            TokenInfo::Import => TokenPrecedence::None,
            TokenInfo::Let => TokenPrecedence::None,
            TokenInfo::Mut => TokenPrecedence::None,
            TokenInfo::Fn => TokenPrecedence::None,
            TokenInfo::As => TokenPrecedence::Cast,
            TokenInfo::If => TokenPrecedence::None,
            TokenInfo::Else => TokenPrecedence::None,
            TokenInfo::Struct => TokenPrecedence::None,
            TokenInfo::Return => TokenPrecedence::None,
            TokenInfo::XXXPrint => TokenPrecedence::None,
        }
    }
}

impl Display for TokenInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenInfo::End => write!(f, "EOF"),
            TokenInfo::Ident(_) => write!(f, "identifier"),
            TokenInfo::Bool(_) => write!(f, "Bool"),
            TokenInfo::Char(_) => write!(f, "Char"),
            TokenInfo::Int(_) => write!(f, "Int"),
            TokenInfo::Float(_) => write!(f, "Float"),
            TokenInfo::String(_) => write!(f, "String"),
            TokenInfo::Newline => write!(f, "new-line character"),
            TokenInfo::Comma => write!(f, ","),
            TokenInfo::Colon => write!(f, ":"),
            TokenInfo::ParenOpen => write!(f, "("),
            TokenInfo::ParenClose => write!(f, ")"),
            TokenInfo::CurlyOpen => write!(f, "{{"),
            TokenInfo::CurlyClose => write!(f, "}}"),
            TokenInfo::SqrBracketOpen => write!(f, "["),
            TokenInfo::SqrBracketClose => write!(f, "]"),
            TokenInfo::Plus => write!(f, "+"),
            TokenInfo::Dash => write!(f, "-"),
            TokenInfo::Star => write!(f, "*"),
            TokenInfo::Slash => write!(f, "/"),
            TokenInfo::Percent => write!(f, "%"),
            TokenInfo::Bang => write!(f, "!"),
            TokenInfo::Equal => write!(f, "="),
            TokenInfo::Ampersand => write!(f, "&"),
            TokenInfo::Dot => write!(f, "."),
            TokenInfo::DotDot => write!(f, ".."),
            TokenInfo::Import => write!(f, "import"),
            TokenInfo::Let => write!(f, "let"),
            TokenInfo::Mut => write!(f, "mut"),
            TokenInfo::Fn => write!(f, "fn"),
            TokenInfo::As => write!(f, "as"),
            TokenInfo::If => write!(f, "if"),
            TokenInfo::Else => write!(f, "else"),
            TokenInfo::Struct => write!(f, "struct"),
            TokenInfo::Return => write!(f, "return"),
            TokenInfo::XXXPrint => write!(f, "XXXprint"),
        }
    }
}

impl Default for TokenInfo {
    fn default() -> Self {
        Self::End
    }
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum TokenPrecedence {
    None,
    Assignment, // = += -= *= /= &= etc.
    Colon,      // :
    Cast,       // as
    Range,      // .. ...
    Or,         // ||
    And,        // &&
    BitOr,      // |
    Xor,        // ^
    BitAnd,     // &
    Equality,   // == !=
    Comparison, // < > <= >=
    Shift,      // << >>
    Term,       // + -
    Factor,     // * / %
    Unary,      // ! ~
    Call,       // . () []
    Primary,
}

impl TryFrom<u8> for TokenPrecedence {
    type Error = &'static str;

    fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
        let prec = match value {
            0 => Self::None,
            1 => Self::Assignment,
            2 => Self::Colon,
            3 => Self::Cast,
            4 => Self::Range,
            5 => Self::Or,
            6 => Self::And,
            7 => Self::BitOr,
            8 => Self::Xor,
            9 => Self::BitAnd,
            10 => Self::Equality,
            11 => Self::Comparison,
            12 => Self::Shift,
            13 => Self::Term,
            14 => Self::Factor,
            15 => Self::Unary,
            16 => Self::Call,
            17 => Self::Primary,
            _ => return Err("value out of bounds for `TokenPrecedence`."),
        };

        Ok(prec)
    }
}

impl TokenPrecedence {
    pub fn next(self) -> Self {
        let s = self as u8;
        let p = Self::Primary as u8;
        let n = std::cmp::min(p, s + 1);
        n.try_into()
            .expect("Guaranteed that `n` will always be a valid `TokenPrecedence`.")
    }
}

pub struct Tokenizer<'file> {
    loaded_file_idx: usize,
    source: VeryPeekable<Chars<'file>>,
    cur_offset: usize,
    previous_was_newline: bool,
    ended: bool,
    peeked: VecDeque<Token>,
}

impl<'file> Tokenizer<'file> {
    pub(crate) fn new(file: &'file LoadedFile) -> Self {
        Self {
            loaded_file_idx: file.index,
            source: file.source.chars().very_peekable(),
            cur_offset: 0,
            previous_was_newline: true,
            ended: false,
            peeked: VecDeque::new(),
        }
    }

    fn current_location(&self) -> CodeLocation {
        CodeLocation {
            loaded_file_idx: self.loaded_file_idx,
            span: self.cur_offset.into(),
        }
    }

    fn create_location(&self, span: impl Into<SourceSpan>) -> CodeLocation {
        CodeLocation::new(self.loaded_file_idx, span)
    }

    fn update_intertoken_state(&mut self, token: &Token) {
        match token.info {
            TokenInfo::Newline => self.previous_was_newline = true,
            TokenInfo::End => self.ended = true,
            _ => self.previous_was_newline = false,
        }
    }

    fn is_ident_begin(c: char) -> bool {
        c == '_' || c.is_alphabetic()
    }

    fn is_ident_cont(c: char) -> bool {
        Self::is_ident_begin(c) || c.is_ascii_digit()
    }

    fn peek_char(&mut self) -> char {
        self.source.peek(0).copied().unwrap_or_default()
    }

    fn next_char(&mut self) -> char {
        let c = self.source.next().unwrap_or_default();
        if c != '\0' || !self.ended {
            self.cur_offset += 1;
        }

        c
    }

    fn match_char(&mut self, c: char) -> bool {
        if self.peek_char() == c {
            self.next_char();
            return true;
        }

        false
    }

    pub fn peek(&mut self, n: usize) -> Result<&Token> {
        while self.peeked.len() <= n {
            let token = self.next_no_peeking()?;
            self.peeked.push_back(token);
        }

        Ok(&self.peeked[n])
    }

    pub fn next(&mut self) -> Result<Token> {
        self.peeked
            .pop_front()
            .map(Ok)
            .unwrap_or_else(|| self.next_no_peeking())
    }

    fn next_no_peeking(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let c = self.peek_char();
        let token = match c {
            '\'' => self.tokenize_text_literal(true)?,
            '"' => self.tokenize_text_literal(false)?,
            _ if c.is_ascii_digit() => self.tokenize_number()?,
            _ if Self::is_ident_begin(c) => self.tokenize_identifier_or_keyword(),
            _ => self.tokenize_punctuation()?,
        };

        self.update_intertoken_state(&token);

        Ok(token)
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek_char();
            match c {
                '#' => loop {
                    let c = self.next_char();
                    if c == '\n' || c == '\0' {
                        break;
                    }
                },
                '\n' => {
                    if !self.previous_was_newline {
                        break;
                    } else {
                        self.next_char();
                    }
                }
                c if c.is_whitespace() => _ = self.next_char(),
                _ => break,
            }
        }
    }

    fn tokenize_text_literal(&mut self, is_char: bool) -> Result<Token> {
        let delimeter = if is_char { '\'' } else { '"' };

        let span_start = self.cur_offset;

        let dbl_quote = self.next_char();
        assert!(
            dbl_quote == delimeter,
            "[INTERNAL ERR] `tokenize_string` didn't find first `\"`."
        );

        let mut word = String::new();
        while self.peek_char() != delimeter && self.peek_char() != '\0' {
            let c = self.next_char();
            match c {
                '\n' => {
                    return Err(SourceError::new(
                        "Newline encountered before string literal was terminated.",
                        self.current_location(),
                        "Expected a `\"` here.",
                    )
                    .into())
                }
                '\\' => todo!(),
                _ => word.push(c),
            }
        }

        if !self.match_char(delimeter) {
            let err = if is_char {
                SourceError::new(
                    "Unterminated char literal.",
                    CodeLocation::new(self.loaded_file_idx, (span_start, word.len())),
                    "This is where the char literal starts.",
                )
            } else {
                SourceError::new(
                    "Unterminated string literal.",
                    CodeLocation::new(self.loaded_file_idx, (span_start, word.len())),
                    "This is where the string literal starts.",
                )
            };

            return Err(err.into());
        }

        let loc = self.create_location((span_start, word.len()));

        if is_char && word.len() != 1 {
            return Err(SourceError::new(
                "Too many chaaracters in `Char` literal.",
                loc,
                "Character literals can only contain one character.",
            )
            .into());
        }

        let tok = if is_char {
            Token::new(loc, TokenInfo::Char(word.chars().next().expect("[INTERNAL ERR] Character literal expected to be guarenteed a length of 1 at this point.")))
        } else {
            Token::new(loc, TokenInfo::String(word))
        };

        Ok(tok)
    }

    fn tokenize_number(&mut self) -> Result<Token> {
        let span_start = self.cur_offset;

        let mut word = String::new();
        while self.peek_char().is_ascii_digit() {
            let c = self.next_char();
            if c == '_' {
                continue;
            }

            word.push(c);
        }

        let info = if self.peek_char() == '.'
            && self.source.peek(1).filter(|c| c.is_ascii_digit()).is_some()
        {
            word.push('.');
            self.next_char();

            while self.peek_char().is_ascii_digit() {
                let c = self.next_char();
                if c == '_' {
                    continue;
                }
                word.push(c);
            }

            let f = word.parse().expect(
                "The above processing should guarentee that this is a valid float literal.",
            );
            TokenInfo::Float(f)
        } else {
            let n = word
                .parse()
                .expect("The above processing should guarentee that this is a valid int literal.");
            TokenInfo::Int(n)
        };

        let tok_loc = self.create_location((span_start, word.len()));
        Ok(Token::new(tok_loc, info))
    }

    fn tokenize_identifier_or_keyword(&mut self) -> Token {
        let span_start = self.cur_offset;

        let mut word = String::new();
        while Self::is_ident_cont(self.peek_char()) {
            word.push(self.next_char());
        }

        let tok_loc = self.create_location((span_start, word.len()));
        match word.as_str() {
            "true" => Token::new(tok_loc, TokenInfo::Bool(true)),
            "false" => Token::new(tok_loc, TokenInfo::Bool(false)),
            "import" => Token::new(tok_loc, TokenInfo::Import),
            "let" => Token::new(tok_loc, TokenInfo::Let),
            "mut" => Token::new(tok_loc, TokenInfo::Mut),
            "fn" => Token::new(tok_loc, TokenInfo::Fn),
            "as" => Token::new(tok_loc, TokenInfo::As),
            "if" => Token::new(tok_loc, TokenInfo::If),
            "else" => Token::new(tok_loc, TokenInfo::Else),
            "struct" => Token::new(tok_loc, TokenInfo::Struct),
            "return" => Token::new(tok_loc, TokenInfo::Return),
            "XXXprint" => Token::new(tok_loc, TokenInfo::XXXPrint),
            _ => Token::new(tok_loc, TokenInfo::Ident(word)),
        }
    }

    fn tokenize_punctuation(&mut self) -> Result<Token> {
        let span_start = self.cur_offset;

        let op = match self.next_char() {
            // Delimeters
            '\0' => TokenInfo::End,
            '\n' => TokenInfo::Newline,
            ',' => TokenInfo::Comma,
            ':' => TokenInfo::Colon,
            '(' => TokenInfo::ParenOpen,
            ')' => TokenInfo::ParenClose,
            '{' => TokenInfo::CurlyOpen,
            '}' => TokenInfo::CurlyClose,
            '[' => TokenInfo::SqrBracketOpen,
            ']' => TokenInfo::SqrBracketClose,

            // Operators
            '+' => TokenInfo::Plus,
            '-' => TokenInfo::Dash,
            '*' => TokenInfo::Star,
            '/' => TokenInfo::Slash,
            '%' => TokenInfo::Percent,
            '!' => TokenInfo::Bang,
            '=' => TokenInfo::Equal,
            '&' => TokenInfo::Ampersand,
            '.' => {
                if self.match_char('.') {
                    TokenInfo::DotDot
                } else {
                    TokenInfo::Dot
                }
            }

            _ => {
                return Err(SourceError::new(
                    "Encountered an invalid operator",
                    self.create_location(span_start..self.cur_offset),
                    "This is an invalid operator.",
                )
                .into())
            }
        };

        let tok_loc = self.create_location(span_start..self.cur_offset);
        Ok(Token::new(tok_loc, op))
    }
}
