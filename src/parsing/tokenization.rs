use std::{
    str::Chars,
    iter::Peekable,
};

use crate::LoadedFile;

pub(crate) fn tokenize_file(file: &LoadedFile) -> Result<Vec<Token>, &'static str> {
    Tokenizer::new(file).collect::<Result<_, _>>()
}

#[derive(Debug)]
pub struct Token {
    loc: CodeLocation,
    data: TokenData,
}

#[derive(Debug)]
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

    XXX_print,
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

    fn next_token(&mut self) -> Result<Token, &'static str> {
        self.skip_whitespace();
        todo!();
    }

    fn skip_whitespace(&mut self) {
        loop {
            if self.source.next_if(|c| c.is_whitespace()).is_none() {
                break;
            }
        }
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

