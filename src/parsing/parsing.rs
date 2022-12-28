use enum_tags::TaggedEnum;

use crate::{
    interp::{LoadedFile, ParsedFile},
    ir::ast::{
        Ast, AstBinaryKind, AstBlockKind, AstInfo, AstInfoFn, AstInfoImport, AstInfoVar,
        AstUnaryKind, Queued, VariableInitializer,
    },
    parsing::tokenization::*,
    util::lformat,
};

pub fn parse_file(file: &LoadedFile) -> Result<ParsedFile, &'static str> {
    let mut nodes = vec![];
    let mut parser = Parser::new(Tokenizer::new(file));

    while !parser.peek_token(0)?.is_end() {
        let node = parser.parse_declaration()?;

        let queued = Queued::new(node);
        nodes.push(queued);

        parser.skip_newline()?;
    }

    Ok(ParsedFile {
        filepath: file.filepath.clone(),
        ast: nodes,
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

    fn match_ident(&mut self, ident: impl AsRef<str>) -> Result<bool, &'static str> {
        if self.check_token(TokenInfoTag::Ident)? {
            let ident_tok = self.peek_token(0).expect("Already peeked");
            match &ident_tok.info {
                TokenInfo::Ident(found) if ident.as_ref() == found => {
                    self.next_token().expect("Already peeked");
                    Ok(true)
                }
                _ => Ok(false),
            }
        } else {
            Ok(false)
        }
    }
}

impl<'file> Parser<'file> {
    fn parse_declaration(&mut self) -> ParseResult {
        let token = self.peek_token(0)?;
        match token.info {
            TokenInfo::Fn => self.parse_fn_decl(),
            TokenInfo::Let | TokenInfo::Mut => self.parse_var_decl(),
            TokenInfo::Import => self.parse_import_decl(),
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

        let returns = if self.match_token(TokenInfoTag::Colon)? {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let body = self.parse_block(AstBlockKind::Block, TokenInfoTag::CurlyClose)?;

        Ok(Ast::new(
            fn_tok,
            AstInfo::Fn(Box::new(AstInfoFn {
                ident,
                params,
                returns,
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
                Some(self.parse_comma_separated_expressions(
                    AstBlockKind::Comma,
                    TokenInfoTag::Newline,
                    Some(expr),
                )?)
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

    fn parse_import_decl(&mut self) -> ParseResult {
        let import_tok = self.expect_token(
            TokenInfoTag::Import,
            "Expected `import` keyword to begin import declaration.",
        )?;

        let path_tok = self.expect_token(
            TokenInfoTag::String,
            "Expected file path after `import` keyword in import declaration.",
        )?;
        let path = Ast::new_literal(path_tok);

        let renamer = if self.match_token(TokenInfoTag::As)? {
            let renamer_tok = self.expect_token(
                TokenInfoTag::Ident,
                "Expected an identifier after `as` keyword in import declaration.",
            )?;
            Some(Ast::new_literal(renamer_tok))
        } else {
            None
        };

        let exposing = if self.match_ident("exposing")? {
            self.expect_token(
                TokenInfoTag::CurlyOpen,
                "Expected `{` after `exposing` contextual keyword in import declaration.",
            )?;

            let items = self.parse_comma_separated_expressions(
                AstBlockKind::Args,
                TokenInfoTag::CurlyClose,
                None,
            )?;
            self.expect_token(
                TokenInfoTag::CurlyClose,
                "Expected `}` to terminate item list of exopsing clause in import declaration.",
            )?;

            Some(items)
        } else {
            None
        };

        Ok(Ast::new(
            import_tok,
            AstInfo::Import(Box::new(AstInfoImport {
                path,
                renamer,
                exposing,
            })),
        ))
    }

    fn parse_statement_or_assignment(&mut self) -> ParseResult {
        let mut node = self.parse_statement()?;

        if self.check_token(TokenInfoTag::Equal)? {
            let token = self.next_token().expect("Already peeked.");
            let prec = token.info.precedence();
            node = self.parse_binary(AstBinaryKind::Assign, token, prec, Box::new(node))?;
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
            TokenInfo::Ident(_)
            | TokenInfo::Int(_)
            | TokenInfo::Float(_)
            | TokenInfo::String(_) => Ok(Ast::new_literal(token)),
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
            TokenInfo::Star => self.parse_unary(AstUnaryKind::Deref, token),
            TokenInfo::Ampersand => self.parse_unary(AstUnaryKind::Ref, token),
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
                let args = self.parse_comma_separated_expressions(
                    AstBlockKind::Args,
                    TokenInfoTag::ParenClose,
                    None,
                )?;
                self.expect_token(
                    TokenInfoTag::ParenClose,
                    "Expected `)` after argument list of function call.",
                )?;
                Ok(Ast::new_binary(
                    AstBinaryKind::Call,
                    token,
                    Box::new(previous),
                    Box::new(args),
                ))
            }
            TokenInfo::SqrBracketOpen => {
                let sub =
                    self.parse_binary(AstBinaryKind::Subscript, token, prec, Box::new(previous))?;
                self.expect_token(
                    TokenInfoTag::SqrBracketClose,
                    "Expected `]` to terminate subscript operation.",
                )?;
                Ok(sub)
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

    fn parse_comma_separated_expressions(
        &mut self,
        kind: AstBlockKind,
        terminator: TokenInfoTag,
        initial: Option<Ast>,
    ) -> ParseResult {
        let mut exprs = vec![];
        let token: Token;

        if let Some(initial) = initial {
            token = initial.token.clone();
            exprs.push(initial)
        } else {
            token = self.peek_token(0)?.clone();
        }

        while !self.skip_check_token(terminator)? && !self.check_token(TokenInfoTag::End)? {
            let expr = self.parse_expression()?;
            exprs.push(expr);

            if !self.skip_match_token(TokenInfoTag::Comma)? {
                break;
            }
        }

        Ok(Ast::new_block(kind, token, exprs))
    }
}
