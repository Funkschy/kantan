use std::io::{self, Write};

mod cli;
mod parse;
mod resolve;
mod types;

use self::{
    parse::{ast::Program, lexer::Lexer, parser::Parser, Span, Spanned},
    resolve::Resolver,
};

pub(crate) use self::cli::*;

#[derive(Debug, Eq, PartialEq)]
pub struct Source {
    pub name: String,
    pub code: String,
}

impl Source {
    pub fn new(name: &str, code: &str) -> Self {
        Source {
            name: name.to_owned(),
            code: code.to_owned(),
        }
    }

    pub fn slice(&self, span: Span) -> &str {
        &self.code[span.start..=span.end]
    }
}

pub fn compile<W: Write>(sources: &[Source], writer: &mut W) -> io::Result<()> {
    #[cfg(windows)]
    {
        if let Err(code) = ansi_term::enable_ansi_support() {
            eprintln!(
                "Could not initialise windows ansi support. Error code: {}",
                code
            );
        }
    }

    let (parse_trees, err_count) =
        sources
            .iter()
            .fold((vec![], 0), |(mut asts, err_count), source| {
                let lexer = Lexer::new(&source.code);
                let mut parser = Parser::new(lexer);
                let prg = parser.parse();
                asts.push(prg);
                (asts, err_count + parser.err_count)
            });

    let ast_sources = parse_trees
        .iter()
        .zip(sources.iter())
        .collect::<Vec<(&Program, &Source)>>();

    // let imports: Vec<Spanned<&str>> = prg
    //     .0
    //     .iter()
    //     .filter_map(|top_lvl| {
    //         if let Stmt::Import { name } = top_lvl {
    //             Some(*name)
    //         } else {
    //             None
    //         }
    //     })
    //     .collect();

    if err_count == 0 {
        for (ast, source) in &ast_sources {
            let mut resolver = Resolver::new(source);
            let errors: Vec<String> = resolver
                .resolve(ast)
                .iter()
                .map(|err| err.to_string())
                .collect();

            print_error(&errors.join("\n\n"), writer)?;
        }
    } else {
        for (ast, source) in &ast_sources {
            report_errors(source, ast, writer)?;
        }
    }

    Ok(())
}
