use enum_tags::TaggedEnum;

use crate::{
    ir::ast::{Ast, AstBinaryKind, AstBlockKind, AstInfo, AstInfoFn, AstUnaryKind},
    parsing::tokenization::*,
    util::{
        lformat,
        structures::{LoadedFile, ParsedFile},
    },
};

pub fn parse_file(file: &LoadedFile) -> Result<ParsedFile, &'static str> {
    let mut nodes = vec![];
    let tokenizer = Tokenizer::new(file);
    let mut parser = Parser::new(tokenizer);

    while !parser.peek_token(0)?.is_end() {
        let node = parser.parse_declaration()?;
        nodes.push(node);

        parser.skip_newline()?;
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

    fn expect_token(
        &mut self,
        kind: TokenInfoTag,
        err: &'static str,
    ) -> Result<Token, &'static str> {
        if self.check_token(kind)? {
            return Ok(self.next_token().expect("Already peeked."));
        }

        Err(err)
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
            TokenInfo::Fn => self.parse_fn_decl(),
            TokenInfo::Let | TokenInfo::Mut => todo!(),
            TokenInfo::Import => todo!(),
            _ => self.parse_statement_or_assignment(),
        }
    }

    fn parse_fn_decl(&mut self) -> ParseResult {
        let fn_tok = self.expect_token(
            TokenInfoTag::Fn,
            "Expected `fn` keyword to begin function declaration.",
        )?;

        let ident_tok = self.expect_token(
            TokenInfoTag::Ident,
            "Expected an identifier after `fn` keyword.",
        )?;
        let ident = Ast::new_literal(ident_tok);

        let params = self.parse_params()?;

        let body = self.parse_block(AstBlockKind::Block, TokenInfoTag::CurlyClose)?;

        Ok(Ast::new(
            fn_tok,
            AstInfo::Fn(Box::new(AstInfoFn {
                ident,
                params,
                body,
            })),
        ))
    }

    fn parse_params(&mut self) -> ParseResult {
        let tok = self.expect_token(
            TokenInfoTag::ParenOpen,
            "Expected `(` to begin function parameter list.",
        )?;
        let mut params = vec![];

        loop {
            if self.check_token(TokenInfoTag::ParenClose)? {
                break;
            }

            let param_ident_tok = self.expect_token(
                TokenInfoTag::Ident,
                "Expected an identifier for function parameter.",
            )?;
            let param_ident = Ast::new_literal(param_ident_tok);
            let colon_tok = self.expect_token(
                TokenInfoTag::Colon,
                "Expected `:` after function paramter name.",
            )?;
            let param_typ = self.parse_expression()?;
            let param = Ast::new_binary(
                AstBinaryKind::Param,
                colon_tok,
                Box::new(param_ident),
                Box::new(param_typ),
            );

            params.push(param);

            if !self.match_token(TokenInfoTag::Comma)? || self.match_token(TokenInfoTag::End)? {
                break;
            }
        }

        self.expect_token(
            TokenInfoTag::ParenClose,
            "Expected `)` to terminate function parameter list.",
        )?;

        Ok(Ast::new_block(AstBlockKind::Params, tok, params))
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
        let node = match token.info {
            TokenInfo::XXXPrint => {
                let token = self.next_token().expect("Already peeked");
                self.parse_unary(AstUnaryKind::XXXPrint, token)?
            }
            _ => self.parse_expression()?,
        };

        self.expect_token(
            TokenInfoTag::Newline,
            "Statements must be placed on their own line.",
        )?;

        Ok(node)
    }

    fn parse_expression(&mut self) -> ParseResult {
        self.parse_precedence(TokenPrecedence::Assignment)
    }

    fn parse_precedence(&mut self, prec: TokenPrecedence) -> ParseResult {
        let token = self.next_token()?;
        if token.is_end() {
            return Err("Expected an expression.");
        }

        let mut previous = self.parse_prefix(token)?;
        while prec <= self.peek_token(0)?.info.precedence() {
            let token = self.next_token()?;
            if token.is_end() {
                return Err("Incomplete expression.");
            }

            previous = self.parse_infix(token, previous)?;
        }

        Ok(previous)
    }

    fn parse_prefix(&mut self, token: Token) -> ParseResult {
        match token.info {
            TokenInfo::Ident(_) | TokenInfo::Int(_) | TokenInfo::Float(_) => {
                Ok(Ast::new_literal(token))
            }
            TokenInfo::ParenOpen => {
                let node = self.parse_expression()?;
                self.expect_token(
                    TokenInfoTag::ParenClose,
                    "Expected `)` to terminate parenthesized expression.",
                )?;
                Ok(node)
            }
            TokenInfo::Bang => self.parse_unary(AstUnaryKind::Not, token),
            TokenInfo::Dash => self.parse_unary(AstUnaryKind::Neg, token),
            TokenInfo::XXXPrint => self.parse_unary(AstUnaryKind::XXXPrint, token),
            _ => Err(lformat!(
                "Encountered a non-prefix token `{:?}` in prefix position.",
                token.info
            )),
        }
    }

    fn parse_infix(&mut self, token: Token, previous: Ast) -> ParseResult {
        let prec = token.info.precedence();
        match token.info {
            TokenInfo::Colon => todo!(),
            TokenInfo::ParenOpen => {
                self.parse_binary(AstBinaryKind::Call, token, prec, Box::new(previous))
            }
            TokenInfo::SqrBracketOpen => {
                self.parse_binary(AstBinaryKind::Subscript, token, prec, Box::new(previous))
            }
            TokenInfo::Plus => {
                self.parse_binary(AstBinaryKind::Add, token, prec, Box::new(previous))
            }
            TokenInfo::Dash => {
                self.parse_binary(AstBinaryKind::Sub, token, prec, Box::new(previous))
            }
            TokenInfo::Star => {
                self.parse_binary(AstBinaryKind::Mul, token, prec, Box::new(previous))
            }
            TokenInfo::Slash => {
                self.parse_binary(AstBinaryKind::Div, token, prec, Box::new(previous))
            }
            TokenInfo::Percent => {
                self.parse_binary(AstBinaryKind::Mod, token, prec, Box::new(previous))
            }
            _ => Err(lformat!("Encountered a non-infix token `{:?}` in infix position.", token.info)),
        }
    }

    fn parse_unary(&mut self, kind: AstUnaryKind, token: Token) -> ParseResult {
        self.skip_newline()?;
        let sub_expression = self.parse_precedence(TokenPrecedence::Unary)?;
        Ok(Ast::new_unary(kind, token, Box::new(sub_expression)))
    }

    fn parse_binary(
        &mut self,
        kind: AstBinaryKind,
        token: Token,
        prec: TokenPrecedence,
        lhs: Box<Ast>,
    ) -> ParseResult {
        self.skip_newline()?;
        let rhs = self.parse_precedence(prec)?;
        Ok(Ast::new_binary(kind, token, lhs, Box::new(rhs)))
    }

    fn parse_block(&mut self, kind: AstBlockKind, terminator: TokenInfoTag) -> ParseResult {
        let token = self.next_token()?;

        let mut nodes = vec![];
        while !self.match_token(terminator)? && !self.check_token(TokenInfoTag::End)? {
            self.skip_newline()?;
            let node = self.parse_declaration()?;
            nodes.push(node);
        }

        Ok(Ast::new_block(kind, token, nodes))
    }
}
