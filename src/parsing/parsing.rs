use enum_tags::TaggedEnum;

use crate::{
    ir::ast::{Ast, AstBinaryKind, AstBlockKind, AstInfo, AstUnaryKind},
    parsing::tokenization::*,
    util::structures::{LoadedFile, ParsedFile},
};

pub fn parse_file(file: &LoadedFile) -> Result<ParsedFile, &'static str> {
    let mut nodes = vec![];
    let tokenizer = Tokenizer::new(file);
    let mut parser = Parser::new(tokenizer);

    while !parser.tokenizer.is_finished() {
        let node = parser.parse_declaration()?;
        nodes.push(node);
    }

    Ok(ParsedFile {
        filepath: file.filepath.clone(),
        ast: Ast::new_block(
            AstBlockKind::Program,
            Token {
                loc: CodeLocation { ln: 1, ch: 1 },
                info: TokenInfo::End,
            },
            nodes,
        ),
    })
}

type ParseResult = Result<Ast, &'static str>;

struct Parser<'file> {
    tokenizer: Tokenizer<'file>,
}

impl<'file> Parser<'file> {
    fn new(tokenizer: Tokenizer<'file>) -> Self {
        Self { tokenizer }
    }
}

impl<'file> Parser<'file> {
    fn peek_token(&mut self, n: usize) -> Result<&Token, &'static str> {
        self.tokenizer.peek(n)
    }

    fn next_token(&mut self) -> Result<Token, &'static str> {
        self.tokenizer.next()
    }

    fn check_token(&mut self, kind: TokenInfoTag) -> Result<bool, &'static str> {
        self.peek_token(0).map(|tok| tok.info.tag() == kind)
    }

    fn match_token(&mut self, kind: TokenInfoTag) -> Result<bool, &'static str> {
        if self.check_token(kind)? {
            _ = self.next_token().expect("Already peeked.");
            return Ok(true);
        }

        Ok(false)
    }

    fn skip_newline(&mut self) -> Result<(), &'static str> {
        self.match_token(TokenInfoTag::Newline)?;
        Ok(())
    }
}

impl<'file> Parser<'file> {
    fn parse_declaration(&mut self) -> ParseResult {
        let token = self.peek_token(0)?;
        match token.info {
            TokenInfo::Fn => todo!(),
            TokenInfo::Let | TokenInfo::Mut => todo!(),
            TokenInfo::Import => todo!(),
            _ => self.parse_statement_or_assignment(),
        }
    }

    fn parse_fn_decl(&mut self) -> ParseResult {
        todo!()
    }

    fn parse_var_decl(&mut self) -> ParseResult {
        todo!()
    }


    fn parse_statement_or_assignment(&mut self) -> ParseResult {
        let node = self.parse_statement()?;
        if matches!(node.info, AstInfo::Binary(AstBinaryKind::Assign, _, _)) {
            return Err("Cannot assign in expression context.");
        }

        Ok(node)
    }

    fn parse_statement(&mut self) -> ParseResult {
        let token = self.peek_token(0)?;
        match token.info {
            TokenInfo::XXXPrint => {
                let token = self.next_token().expect("Already peeked");
                self.parse_unary(AstUnaryKind::XXXPrint, token)
            }
            _ => self.parse_expression(),
        }
    }

    fn parse_expression(&mut self) -> ParseResult {
        self.parse_precedence(TokenPrecedence::Assignment)
    }

    fn parse_precedence(&mut self, prec: TokenPrecedence) -> ParseResult {
        let token = self.next_token()?;
        if token.is_end() {
            return Err("Unterminated expression.");
        }

        let mut previous = self.parse_prefix(token)?;
        while prec > self.peek_token(0)?.info.precedence() {
            let token = self.next_token()?;
            if token.is_end() {
                return Err("Unterminated expression.");
            }

            previous = self.parse_infix(token, previous)?;
        }

        Ok(previous)
    }

    fn parse_prefix(&mut self, token: Token) -> ParseResult {
        todo!()
    }

    fn parse_infix(&mut self, token: Token, previous: Ast) -> ParseResult {
        todo!()
    }

    fn parse_unary(&mut self, kind: AstUnaryKind, token: Token) -> ParseResult {
        self.skip_newline()?;
        let sub_expression = self.parse_precedence(TokenPrecedence::Unary)?;
        Ok(Ast::new_unary(kind, token, Box::new(sub_expression)))
    }

    fn parse_binary(&mut self, kind: AstBinaryKind, token: Token, prec: TokenPrecedence, lhs: Box<Ast>) -> ParseResult {
        self.skip_newline()?;
        let rhs = self.parse_precedence(prec)?;
        Ok(Ast::new_binary(kind, token, lhs, Box::new(rhs)))
    }
}
