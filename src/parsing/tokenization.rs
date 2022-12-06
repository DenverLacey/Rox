use std::{iter::Peekable, str::Chars};

use crate::LoadedFile;

pub(crate) fn tokenize_file(file: &LoadedFile) -> Result<Vec<Token>, &'static str> {
    Tokenizer::new(file).collect()
}

#[derive(Debug)]
pub struct Token {
    loc: CodeLocation,
    data: TokenData,
}

impl Token {
    fn new(loc: CodeLocation, data: TokenData) -> Self {
        Self { loc, data }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct CodeLocation {
    ln: usize,
    ch: usize,
}

#[derive(Debug)]
pub enum TokenData {
    End,

    // Literals
    Ident(String), // @IMPROVE: This could probably be a &str with some lifetime
    Int(i64),

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

    // Keywords
    Import,
    Let,
    Mut,
    Fn,

    XXXPrint,
}

struct Tokenizer<'file> {
    source: Peekable<Chars<'file>>,
    cur_loc: CodeLocation,
    previous_was_newline: bool,
    ended: bool,
}

impl<'file> Tokenizer<'file> {
    fn new(file: &'file LoadedFile) -> Self {
        Self {
            source: file.source.chars().peekable(),
            cur_loc: CodeLocation { ln: 1, ch: 1 },
            previous_was_newline: true,
            ended: false,
        }
    }

    fn peek_char(&mut self) -> char {
        self.source.peek().map(|c| *c).unwrap_or_default()
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

        return c;
    }

    fn next_token(&mut self) -> Result<Token, &'static str> {
        self.skip_whitespace();

        let c = self.peek_char();
        match c {
            _ if c.is_ascii_digit() => self.tokenize_number(),
            _ if c.is_alphabetic() => Ok(self.tokenize_identifier_or_keyword()),
            _ => self.tokenize_punctuation(),
        }
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

        // @TODO: Handle floating point numbers
        let n = word.parse().map_err(|_| "Failed to parse number.")?;

        return Ok(Token::new(tok_loc, TokenData::Int(n)));
    }

    fn tokenize_identifier_or_keyword(&mut self) -> Token {
        let tok_loc = self.cur_loc;

        let mut word = String::new();
        while self.peek_char().is_alphanumeric() {
            // @TODO: Handle underscores
            word.push(self.next_char());
        }

        match word.as_str() {
            "import" => Token::new(tok_loc, TokenData::Import),
            "let" => Token::new(tok_loc, TokenData::Let),
            "mut" => Token::new(tok_loc, TokenData::Mut),
            "fn" => Token::new(tok_loc, TokenData::Fn),
            "XXXprint" => Token::new(tok_loc, TokenData::XXXPrint),
            _ => Token::new(tok_loc, TokenData::Ident(word)),
        }
    }

    fn tokenize_punctuation(&mut self) -> Result<Token, &'static str> {
        let tok_loc = self.cur_loc;

        let op = match self.next_char() {
            // Delimeters
            '\0' => TokenData::End,
            '\n' => TokenData::Newline,
            ',' => TokenData::Comma,
            ':' => TokenData::Colon,
            '(' => TokenData::ParenOpen,
            ')' => TokenData::ParenClose,
            '{' => TokenData::CurlyOpen,
            '}' => TokenData::CurlyClose,
            '[' => TokenData::SqrBracketOpen,
            ']' => TokenData::SqrBracketClose,

            // Operators
            '+' => TokenData::Plus,
            '-' => TokenData::Dash,
            '*' => TokenData::Star,
            '/' => TokenData::Slash,
            '%' => TokenData::Percent,

            _ => return Err("Invalid operator."),
        };

        return Ok(Token::new(tok_loc, op));
    }
}

impl<'file> Iterator for Tokenizer<'file> {
    type Item = Result<Token, &'static str>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.ended {
            return None;
        }

        let token = self.next_token();
        if let Ok(token) = &token {
            match token.data {
                TokenData::Newline => self.previous_was_newline = true,
                TokenData::End => self.ended = true,
                _ => self.previous_was_newline = false,
            }
        }

        return Some(token);
    }
}

