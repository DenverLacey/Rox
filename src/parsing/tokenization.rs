use std::collections::VecDeque;
use std::str::Chars;

use enum_tags::*;

use crate::util::iter::{VeryPeekable, VeryPeekableIterExt};
use crate::util::structures::LoadedFile;

#[derive(Debug)]
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

#[derive(Clone, Copy, Debug)]
pub struct CodeLocation {
    pub ln: usize,
    pub ch: usize,
}

#[derive(Debug, Tag)]
pub enum TokenInfo {
    End,

    // Literals
    Ident(String), // @IMPROVE: This could probably be a &str with some lifetime
    Int(i64),
    Float(f64),

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

    // Keywords
    Import,
    Let,
    Mut,
    Fn,

    XXXPrint,
}

impl TokenInfo {
    pub const fn precedence(&self) -> TokenPrecedence {
        match *self {
            TokenInfo::End => TokenPrecedence::None,
            TokenInfo::Ident(_) => TokenPrecedence::None,
            TokenInfo::Int(_) => TokenPrecedence::None,
            TokenInfo::Float(_) => TokenPrecedence::None,
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
            TokenInfo::Import => TokenPrecedence::None,
            TokenInfo::Let => TokenPrecedence::None,
            TokenInfo::Mut => TokenPrecedence::None,
            TokenInfo::Fn => TokenPrecedence::None,
            TokenInfo::XXXPrint => TokenPrecedence::None,
        }
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

pub struct Tokenizer<'file> {
    source: VeryPeekable<Chars<'file>>,
    cur_loc: CodeLocation,
    previous_was_newline: bool,
    ended: bool,
    peeked: VecDeque<Token>,
}

impl<'file> Tokenizer<'file> {
    pub(crate) fn new(file: &'file LoadedFile) -> Self {
        Self {
            source: file.source.chars().very_peekable(),
            cur_loc: CodeLocation { ln: 1, ch: 1 },
            previous_was_newline: true,
            ended: false,
            peeked: VecDeque::new(),
        }
    }

    pub(crate) fn is_finished(&self) -> bool {
        self.ended
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
        match c {
            '\0' => {
                if !self.ended {
                    self.cur_loc.ch += 1;
                }
            }
            '\n' => {
                self.cur_loc.ln += 1;
                self.cur_loc.ch = 1;
            }
            _ => self.cur_loc.ch += 1,
        }

        c
    }

    pub fn peek(&mut self, n: usize) -> Result<&Token, &'static str> {
        while self.peeked.len() <= n {
            let token = self.next_no_peeking()?;
            self.peeked.push_back(token);
        }

        Ok(&self.peeked[n])
    }

    pub fn next(&mut self) -> Result<Token, &'static str> {
        self.peeked
            .pop_front()
            .map(|tok| Ok(tok))
            .unwrap_or_else(|| self.next_no_peeking())
    }

    fn next_no_peeking(&mut self) -> Result<Token, &'static str> {
        self.skip_whitespace();

        let c = self.peek_char();
        let token = match c {
            _ if c.is_ascii_digit() => self.tokenize_number()?,
            _ if Self::is_ident_begin(c) => Ok(self.tokenize_identifier_or_keyword())?,
            _ => self.tokenize_punctuation()?,
        };

        match token.info {
            TokenInfo::Newline => self.previous_was_newline = true,
            TokenInfo::End => self.ended = true,
            _ => self.previous_was_newline = false,
        }

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
                        _ = self.next_char();
                    }
                }
                c if c.is_whitespace() => _ = self.next_char(),
                _ => break,
            }
        }
    }

    fn tokenize_number(&mut self) -> Result<Token, &'static str> {
        let tok_loc = self.cur_loc;

        let mut word = String::new();
        while self.peek_char().is_ascii_digit() {
            word.push(self.next_char());
        }

        let info = if self.peek_char() == '.'
            && self.source.peek(1).filter(|c| c.is_ascii_digit()).is_some()
        {
            word.push('.');
            _ = self.next_char();

            while self.peek_char().is_ascii_digit() {
                word.push(self.next_char());
            }

            let f = word.parse().map_err(|_| "Failed to parse number.")?;
            TokenInfo::Float(f)
        } else {
            let n = word.parse().map_err(|_| "Failed to parse number.")?;
            TokenInfo::Int(n)
        };

        Ok(Token::new(tok_loc, info))
    }

    fn tokenize_identifier_or_keyword(&mut self) -> Token {
        let tok_loc = self.cur_loc;

        let mut word = String::new();
        while Self::is_ident_cont(self.peek_char()) {
            word.push(self.next_char());
        }

        match word.as_str() {
            "import" => Token::new(tok_loc, TokenInfo::Import),
            "let" => Token::new(tok_loc, TokenInfo::Let),
            "mut" => Token::new(tok_loc, TokenInfo::Mut),
            "fn" => Token::new(tok_loc, TokenInfo::Fn),
            "XXXprint" => Token::new(tok_loc, TokenInfo::XXXPrint),
            _ => Token::new(tok_loc, TokenInfo::Ident(word)),
        }
    }

    fn tokenize_punctuation(&mut self) -> Result<Token, &'static str> {
        let tok_loc = self.cur_loc;

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

            _ => return Err("Invalid operator."),
        };

        Ok(Token::new(tok_loc, op))
    }
}
