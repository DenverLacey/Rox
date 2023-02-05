use enum_tags::TaggedEnum;

use crate::{
    interp::{LoadedFile, ParsedFile},
    ir::{
        annotations::{self, Annotations},
        ast::{
            Ast, AstBinaryKind, AstBlockKind, AstInfo, AstInfoFn, AstInfoIf, AstInfoImport,
            AstInfoStruct, AstInfoTypeSignature, AstInfoVar, AstOptionalKind, AstUnaryKind, Queued,
        },
    },
    parsing::tokenization::*,
    util::errors::{error, Result, SourceError},
};

pub fn parse_file(file: &LoadedFile) -> Result<ParsedFile> {
    let mut nodes = vec![];
    let mut parser = Parser::new(Tokenizer::new(file));

    while !parser.peek_token(0)?.is_end() {
        let node = parser.parse_declaration()?;

        let queued = Queued::new(node);
        nodes.push(queued);

        parser.skip_newline()?;
    }

    Ok(ParsedFile {
        index: file.index,
        filepath: file.filepath.clone(),
        ast: nodes,
    })
}

type ParseResult = Result<Ast>;

struct Parser<'file> {
    tokenizer: Tokenizer<'file>,
    current_annotations: Annotations,
}

impl<'file> Parser<'file> {
    fn new(tokenizer: Tokenizer<'file>) -> Self {
        Self {
            tokenizer,
            current_annotations: Default::default(),
        }
    }
}

impl<'file> Parser<'file> {
    fn peek_token(&mut self, n: usize) -> Result<&Token> {
        self.tokenizer.peek(n)
    }

    fn next_token(&mut self) -> Result<Token> {
        self.tokenizer.next()
    }

    fn next_token_if_eq(&mut self, kind: TokenInfoTag) -> Result<Option<Token>> {
        if self.check_token(kind)? {
            return Ok(Some(self.next_token().expect("Already peeked")));
        }
        Ok(None)
    }

    fn skip_newline(&mut self) -> Result<()> {
        self.match_token(TokenInfoTag::Newline)?;
        Ok(())
    }

    fn check_token(&mut self, kind: TokenInfoTag) -> Result<bool> {
        self.peek_token(0).map(|tok| tok.info.tag() == kind)
    }

    fn skip_check_token(&mut self, kind: TokenInfoTag) -> Result<bool> {
        self.skip_newline()?;
        self.check_token(kind)
    }

    fn match_token(&mut self, kind: TokenInfoTag) -> Result<bool> {
        if self.check_token(kind)? {
            self.next_token().expect("Already peeked.");
            return Ok(true);
        }

        Ok(false)
    }

    fn skip_match_token(&mut self, kind: TokenInfoTag) -> Result<bool> {
        self.skip_newline()?;
        self.match_token(kind)
    }

    fn peek_match_token(&mut self, kind: TokenInfoTag) -> Result<bool> {
        let peek0 = self.peek_token(0)?;
        if peek0.info.tag() == kind {
            self.next_token().expect("Already been peeked.");
            return Ok(true);
        }

        if peek0.info.tag() != TokenInfoTag::Newline {
            return Ok(false);
        }

        let peek1 = self.peek_token(1)?;
        if peek1.info.tag() == kind {
            self.skip_newline()?;
            self.next_token().expect("Already been peeked.");
            return Ok(true);
        }

        Ok(false)
    }

    fn expect_token(&mut self, kind: TokenInfoTag, err: impl Into<String>) -> Result<Token> {
        if self.check_token(kind)? {
            return Ok(self.next_token().expect("Already peeked."));
        }

        let token = self.peek_token(0).unwrap();
        Err(SourceError::new(format!("Unexpected `{}`.", token.info), token.loc, err).into())
    }

    fn skip_expect_token(&mut self, kind: TokenInfoTag, err: &'static str) -> Result<Token> {
        self.skip_newline()?;
        self.expect_token(kind, err)
    }

    fn match_ident(&mut self, ident: impl AsRef<str>) -> Result<bool> {
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

    fn clear_annontations_for_valid(&mut self, valid: Annotations) -> Result<Annotations> {
        let others = self.current_annotations & !valid;
        if others != Annotations::default() {
            // @TODO: Imrpove error message
            return Err(error!(
                "Invalid annotations: valid = `{:?}` but given = `{:?}`",
                valid, self.current_annotations
            ));
        }

        let annons = self.current_annotations;
        self.current_annotations = Annotations::default();

        Ok(annons)
    }
}

impl<'file> Parser<'file> {
    fn parse_declaration(&mut self) -> ParseResult {
        let token = self.peek_token(0)?;
        match token.info {
            TokenInfo::Fn => self.parse_fn_decl(),
            TokenInfo::Let | TokenInfo::Mut => self.parse_var_decl(),
            TokenInfo::Import => self.parse_import_decl(),
            TokenInfo::Percent => {
                self.parse_annotations()?;
                self.parse_declaration()
            }
            TokenInfo::Struct => self.parse_struct_decl(),
            _ => self.parse_statement_or_assignment(),
        }
    }

    fn parse_fn_decl(&mut self) -> ParseResult {
        let fn_annons = self.clear_annontations_for_valid(annotations::FN_ANNOTATIONS)?;

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

        let returns = if self.match_token(TokenInfoTag::Colon)? {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let body = self.parse_block(AstBlockKind::Block, TokenInfoTag::CurlyClose)?;

        Ok(Ast::new(
            fn_tok,
            AstInfo::Fn(Box::new(AstInfoFn {
                annons: fn_annons,
                ident,
                params,
                returns,
                body,
                id: None,
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

        let mut initializers_required = false;
        let mut targets = vec![];
        loop {
            let ident_tok =
                self.skip_expect_token(TokenInfoTag::Ident, "Expected an identifier.")?;
            let ident = Ast::new_literal(ident_tok);

            if self.check_token(TokenInfoTag::Colon)? {
                let colon_tok = self.next_token().expect("Already peeked.");
                let type_constraint = self.parse_expression()?;
                let target = Ast::new_binary(
                    AstBinaryKind::ConstrainedVarDeclTarget,
                    colon_tok,
                    Box::new(ident),
                    Box::new(type_constraint),
                );
                targets.push(target);
            } else {
                targets.push(ident);
                initializers_required = true;
            }

            if self.next_token_if_eq(TokenInfoTag::Comma)?.is_none() {
                break;
            }
        }

        let initializers = if self.match_token(TokenInfoTag::Equal)? {
            let expr = self.parse_expression()?;
            if self.match_token(TokenInfoTag::Comma)? {
                let exprs = self.parse_comma_separated_expressions(
                    AstBlockKind::Comma,
                    TokenInfoTag::Newline,
                    Some(expr),
                )?;

                let AstInfo::Block(_, exprs) = exprs.info else {
                    panic!("[INTERNAL ERR] `parse_comma_separated_expressions` return a non `Block` node.");
                };
                exprs
            } else {
                vec![expr]
            }
        } else {
            vec![]
        };

        let num_targets = targets.len();
        let num_exprs = initializers.len();
        if initializers_required
            && ((num_exprs == 0 && num_targets != 1)
                || (num_exprs != num_targets) && num_exprs != 1)
        {
            // @TODO: Improve error message
            return Err(SourceError::new("Incorrect number of targets for number of expressions.", var_tok.loc, "This variable declaration has incorrect number of targets for the number of expressions.").into());
        }

        let mutable = matches!(var_tok.info, TokenInfo::Mut);
        Ok(Ast::new(
            var_tok,
            AstInfo::Var(Box::new(AstInfoVar {
                mutable,
                targets,
                initializers,
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

    fn parse_struct_decl(&mut self) -> ParseResult {
        let struct_annons = self.clear_annontations_for_valid(annotations::STRUCT_ANNOTATIONS)?;

        let struct_tok = self.expect_token(
            TokenInfoTag::Struct,
            "Expected `struct` keyword to begin struct declaration.",
        )?;

        let ident_tok = self.skip_expect_token(TokenInfoTag::Ident, "Expected an identifier.")?;
        let ident = Ast::new_literal(ident_tok);

        let body = self.parse_struct_body()?;

        Ok(Ast::new(
            struct_tok,
            AstInfo::Struct(Box::new(AstInfoStruct {
                annons: struct_annons,
                ident,
                body,
            })),
        ))
    }

    fn parse_struct_body(&mut self) -> ParseResult {
        let token = self.next_token()?;

        let mut fields = vec![];
        while !self.skip_match_token(TokenInfoTag::CurlyClose)?
            && !self.check_token(TokenInfoTag::End)?
        {
            let ident_tok = self.expect_token(TokenInfoTag::Ident, "Expected field name.")?;
            let ident = Ast::new_literal(ident_tok);

            let colon_tok =
                self.skip_expect_token(TokenInfoTag::Colon, "Expected `:` after field name.")?;

            let typ = self.parse_expression()?;

            let field = Ast::new_binary(
                AstBinaryKind::Field,
                colon_tok,
                Box::new(ident),
                Box::new(typ),
            );
            fields.push(field);
        }

        Ok(Ast::new_block(AstBlockKind::Fields, token, fields))
    }

    fn parse_statement_or_assignment(&mut self) -> ParseResult {
        let mut node = self.parse_statement()?;

        if self.check_token(TokenInfoTag::Equal)? {
            let token = self.next_token().expect("Already peeked.");
            let prec = token.info.precedence();
            node = self.parse_binary(AstBinaryKind::Assign, token, prec, Box::new(node))?;
        }

        self.expect_token(
            TokenInfoTag::Newline,
            "Statements must be placed on their own line.",
        )?;

        Ok(node)
    }

    fn parse_statement(&mut self) -> ParseResult {
        let token = self.peek_token(0)?;
        let node = match token.info {
            TokenInfo::If => self.parse_if_statement()?,
            TokenInfo::Return => self.parse_optional_statement(AstOptionalKind::Return)?,
            TokenInfo::XXXPrint => {
                let token = self.next_token().expect("Already peeked");
                self.parse_unary_with_precedence(
                    AstUnaryKind::XXXPrint,
                    TokenPrecedence::Assignment,
                    token,
                )?
            }
            _ => self.parse_expression()?,
        };

        Ok(node)
    }

    fn parse_if_statement(&mut self) -> ParseResult {
        let if_tok = self.expect_token(
            TokenInfoTag::If,
            "Expected `if` keyword to begin if statement.",
        )?;

        let condition = self.parse_expression()?;
        let then_block = self.parse_block(AstBlockKind::Block, TokenInfoTag::CurlyClose)?;

        let else_block = if self.peek_match_token(TokenInfoTag::Else)? {
            let node = if self.check_token(TokenInfoTag::If)? {
                self.parse_if_statement()?
            } else {
                self.parse_block(AstBlockKind::Block, TokenInfoTag::CurlyClose)?
            };

            Some(node)
        } else {
            None
        };

        Ok(Ast::new(
            if_tok,
            AstInfo::If(Box::new(AstInfoIf {
                condition,
                then_block,
                else_block,
            })),
        ))
    }

    fn parse_optional_statement(&mut self, kind: AstOptionalKind) -> ParseResult {
        let token = self
            .next_token()
            .expect("[INTERNAL ERR] Token failed to be extracted after peek.");

        let sub_expression = if self.check_token(TokenInfoTag::Newline)? {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        Ok(Ast::new_optional(kind, token, sub_expression))
    }

    fn parse_expression(&mut self) -> ParseResult {
        self.parse_precedence(TokenPrecedence::Assignment)
    }

    fn parse_precedence(&mut self, prec: TokenPrecedence) -> ParseResult {
        let token = self.next_token()?;
        if token.is_end() {
            return Err(error!("Expected an expression."));
        }

        let mut previous = self.parse_prefix(token)?;
        while prec <= self.peek_token(0)?.info.precedence() {
            let token = self.next_token()?;
            if token.is_end() {
                return Err(error!("Incomplete expression."));
            }

            previous = self.parse_infix(token, previous)?;
        }

        Ok(previous)
    }

    fn parse_prefix(&mut self, token: Token) -> ParseResult {
        match token.info {
            TokenInfo::Ident(_)
            | TokenInfo::Bool(_)
            | TokenInfo::Char(_)
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
            TokenInfo::Dash => self.parse_unary(AstUnaryKind::Neg, token), // @TODO: Precompute negative numbers
            TokenInfo::Star => self.parse_unary(AstUnaryKind::Deref, token),
            TokenInfo::Ampersand => {
                if self.match_token(TokenInfoTag::Mut)? {
                    self.parse_unary(AstUnaryKind::RefMut, token)
                } else {
                    self.parse_unary(AstUnaryKind::Ref, token)
                }
            }
            TokenInfo::Fn => self.parse_fn_type_signature(token),
            _ => Err(error!(
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
            TokenInfo::Dot => {
                self.parse_binary(AstBinaryKind::MemberAccess, token, prec, Box::new(previous))
            }
            TokenInfo::DotDot => {
                todo!()
            }
            _ => Err(error!(
                "Encountered a non-infix token `{:?}` in infix position.",
                token.info
            )),
        }
    }

    fn parse_unary(&mut self, kind: AstUnaryKind, token: Token) -> ParseResult {
        self.parse_unary_with_precedence(kind, TokenPrecedence::Unary, token)
    }

    fn parse_unary_with_precedence(
        &mut self,
        kind: AstUnaryKind,
        prec: TokenPrecedence,
        token: Token,
    ) -> ParseResult {
        self.skip_newline()?;
        let sub_expression = self.parse_precedence(prec)?;
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

    fn parse_annotations(&mut self) -> Result<()> {
        self.expect_token(
            TokenInfoTag::Percent,
            "Expected `%` to begin annotation list.",
        )?;
        self.skip_expect_token(
            TokenInfoTag::SqrBracketOpen,
            "Expected `[` after `%` in annotation list.",
        )?;

        while !self.skip_check_token(TokenInfoTag::SqrBracketClose)?
            && !self.check_token(TokenInfoTag::End)?
        {
            let tok = self.expect_token(
                TokenInfoTag::Ident,
                "Expected an annotation in annotation list.",
            )?;
            let TokenInfo::Ident(annon) = tok.info else {
                unreachable!();
            };

            let Some(annon) = annotations::TABLE.get(annon.as_str()).copied() else {
                return Err(SourceError::new("Invalid annotation", tok.loc, format!("`{}` is not a valid annotation.", annon)).into());
            };

            self.current_annotations |= annon;

            if !self.skip_match_token(TokenInfoTag::Comma)? {
                break;
            }
        }

        self.expect_token(
            TokenInfoTag::SqrBracketClose,
            "Expected `]` to terminate annotation list.",
        )?;
        self.expect_token(
            TokenInfoTag::Newline,
            "Annotation lists must be on their own line.",
        )?;

        Ok(())
    }

    fn parse_fn_type_signature(&mut self, token: Token) -> ParseResult {
        self.expect_token(TokenInfoTag::ParenOpen, "Expected `(` after `fn` keyword.")?;

        let params = self.parse_comma_separated_expressions(
            AstBlockKind::Params,
            TokenInfoTag::ParenClose,
            None,
        )?;
        self.expect_token(
            TokenInfoTag::ParenClose,
            "Expected `)` to terminate parameter list in function signature.",
        )?;

        let returns = if self.match_token(TokenInfoTag::Colon)? {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let sig = AstInfoTypeSignature::Function(params, returns);
        Ok(Ast::new_type_signature(token, sig))
    }
}
