use crate::{
    ir::ast::{Ast, AstBlockKind},
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

struct Parser<'file> {
    tokenizer: Tokenizer<'file>,
}

impl<'file> Parser<'file> {
    fn new(tokenizer: Tokenizer<'file>) -> Self {
        Self { tokenizer }
    }
}

impl<'file> Parser<'file> {
    fn parse_declaration(&mut self) -> Result<Ast, &'static str> {
        todo!()
    }
}
