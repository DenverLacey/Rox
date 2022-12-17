use enum_tags::TaggedEnum;

use crate::{
    ir::ast::{
        Ast, AstBinaryKind, AstBlockKind, AstInfo, AstInfoFn, AstInfoVar, AstUnaryKind,
        VariableInitializer,
    },
    parsing::tokenization::*,
    util::{
        lformat,
        structures::{LoadedFile, ParsedFile},
    },
};

pub fn parse_file(file: &LoadedFile) -> Result<ParsedFile, &'static str> {
    let mut nodes = vec![];
    let mut parser = Parser::new(Tokenizer::new(file));

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
                loc: CodeLocation { ln: 0, ch: 0 },
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

    fn next_token_if_eq(&mut self, kind: TokenInfoTag) -> Result<Option<Token>, &'static str> {
        if self.check_token(kind)? {
            return Ok(Some(self.next_token().expect("Already peeked")));
        }
        Ok(None)
    }

    fn skip_newline(&mut self) -> Result<(), &'static str> {
        self.match_token(TokenInfoTag::Newline)?;
        Ok(())
    }

    fn check_token(&mut self, kind: TokenInfoTag) -> Result<bool, &'static str> {
        self.peek_token(0).map(|tok| tok.info.tag() == kind)
    }

    fn skip_check_token(&mut self, kind: TokenInfoTag) -> Result<bool, &'static str> {
        self.skip_newline()?;
        self.check_token(kind)
    }

    fn match_token(&mut self, kind: TokenInfoTag) -> Result<bool, &'static str> {
        if self.check_token(kind)? {
            self.next_token().expect("Already peeked.");
            return Ok(true);
        }

        Ok(false)
    }

    fn skip_match_token(&mut self, kind: TokenInfoTag) -> Result<bool, &'static str> {
        self.skip_newline()?;
        self.match_token(kind)
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

    fn skip_expect_token(
        &mut self,
        kind: TokenInfoTag,
        err: &'static str,
    ) -> Result<Token, &'static str> {
        self.skip_newline()?;
        self.expect_token(kind, err)
    }
}

impl<'file> Parser<'file> {
    fn parse_declaration(&mut self) -> ParseResult {
        let token = self.peek_token(0)?;
        match token.info {
            TokenInfo::Fn => self.parse_fn_decl(),
            TokenInfo::Let | TokenInfo::Mut => self.parse_var_decl(),
            TokenInfo::Import => todo!(),
            _ => self.parse_statement_or_assignment(),
        }
    }

    fn parse_fn_decl(&mut self) -> ParseResult {
        let fn_tok = self.expect_token(
            TokenInfoTag::Fn,
            "Expected `fn` keyword to begin function declaration.",
        )?;

        let ident_tok = self.skip_expect_token(
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
        let tok = self.skip_expect_token(
            TokenInfoTag::ParenOpen,
            "Expected `(` to begin function parameter list.",
        )?;
        let mut params = vec![];

        loop {
            if self.skip_check_token(TokenInfoTag::ParenClose)? {
                break;
            }

            let param_ident_tok = self.skip_expect_token(
                TokenInfoTag::Ident,
                "Expected an identifier for function parameter.",
            )?;
            let param_ident = Ast::new_literal(param_ident_tok);
            let colon_tok = self.skip_expect_token(
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

            if !self.skip_match_token(TokenInfoTag::Comma)?
                || self.match_token(TokenInfoTag::End)?
            {
                break;
            }
        }

        self.skip_expect_token(
            TokenInfoTag::ParenClose,
            "Expected `)` to terminate function parameter list.",
        )?;

        Ok(Ast::new_block(AstBlockKind::Params, tok, params))
    }

    fn parse_var_decl(&mut self) -> ParseResult {
        let var_tok = self.next_token()?;
        assert!(
            matches!(var_tok.info, TokenInfo::Let | TokenInfo::Mut),
            "Expected variable declaration to begin with either a `let` or `mut` keyword."
        );

        let ident_tok = self.expect_token(
            TokenInfoTag::Ident,
            "Expected an identifier after `let` or `mut`.",
        )?; // @TODO: Improve error message
        let ident_or_idents = if self.check_token(TokenInfoTag::Comma)? {
            let ident = Ast::new_literal(ident_tok);
            let mut idents = vec![ident];

            while self.next_token_if_eq(TokenInfoTag::Comma)?.is_some() {
                let ident_tok = self.expect_token(
                    TokenInfoTag::Ident,
                    "Expected an identifier after `,` in variable declaration.",
                )?;
                let ident = Ast::new_literal(ident_tok);
                idents.push(ident);
            }

            Ast::new_block(
                AstBlockKind::VarDeclTargets,
                idents[0].token.clone(),
                idents,
            )
        } else {
            Ast::new_literal(ident_tok)
        };

        let type_constraint = if self.match_token(TokenInfoTag::Colon)? {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let init_expr = if self.match_token(TokenInfoTag::Equal)? {
            let expr = self.parse_expression()?;
            if self.match_token(TokenInfoTag::Comma)? {
                Some(self.parse_comma_separated_expressions(AstBlockKind::Comma, Some(expr))?)
            } else {
                Some(expr)
            }
        } else {
            None
        };

        let initializer =
            match (type_constraint, init_expr) {
                (Some(tc), Some(ie)) => VariableInitializer::TypeAndExpr(tc, ie),
                (None, Some(ie)) => VariableInitializer::Expr(ie),
                (Some(tc), None) => VariableInitializer::Type(tc),
                _ => return Err(
                    "Variable declarations must declare a variables type, inital value or both.",
                ),
            };

        let mutable = matches!(var_tok.info, TokenInfo::Mut);
        Ok(Ast::new(
            var_tok,
            AstInfo::Var(Box::new(AstInfoVar {
                mutable,
                targets: ident_or_idents,
                initializer,
            })),
        ))
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
            TokenInfo::Equal => {
                self.parse_binary(AstBinaryKind::Assign, token, prec, Box::new(previous))
            }
            _ => Err(lformat!(
                "Encountered a non-infix token `{:?}` in infix position.",
                token.info
            )),
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
        let rhs = self.parse_precedence(prec.next())?;
        Ok(Ast::new_binary(kind, token, lhs, Box::new(rhs)))
    }

    fn parse_block(&mut self, kind: AstBlockKind, terminator: TokenInfoTag) -> ParseResult {
        let token = self.next_token()?;

        let mut nodes = vec![];
        while !self.skip_match_token(terminator)? && !self.check_token(TokenInfoTag::End)? {
            let node = self.parse_declaration()?;
            nodes.push(node);
        }

        Ok(Ast::new_block(kind, token, nodes))
    }

    fn parse_comma_separated_expressions(&mut self, kind: AstBlockKind, initial: Option<Ast>) -> ParseResult {
        let mut exprs = vec![];
        let token: Token;

        if let Some(initial) = initial {
            token = initial.token.clone();
            exprs.push(initial)
        } else {
            token = self.peek_token(0)?.clone();
        }

        while !self.skip_check_token(TokenInfoTag::End)? {
            let expr = self.parse_expression()?;
            exprs.push(expr);

            if !self.skip_match_token(TokenInfoTag::Comma)? {
                break;
            }
        }

        Ok(Ast::new_block(kind, token, exprs))
    }
}
