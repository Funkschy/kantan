use std::io::{self, Write};

mod cli;
mod parse;
mod resolve;
mod types;

use self::{
    parse::{lexer::Lexer, parser::Parser, Span, Spanned},
    resolve::Resolver,
};

pub(crate) use self::cli::*;

#[derive(Debug, Eq, PartialEq)]
pub struct Source<'input> {
    name: String,
    code: &'input str,
}

impl<'input> Source<'input> {
    pub fn new(name: &str, code: &'input str) -> Self {
        Source {
            name: name.to_owned(),
            code,
        }
    }
}

pub fn compile<W: Write>(source: &Source, writer: &mut W) -> io::Result<()> {
    #[cfg(windows)]
    {
        if let Err(code) = ansi_term::enable_ansi_support() {
            eprintln!(
                "Could not initialise windows ansi support. Error code: {}",
                code
            );
        }
    }

    let lexer = Lexer::new(source.code);
    let mut parser = Parser::new(lexer);

    let prg = parser.parse();
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

    if parser.err_count == 0 {
        let mut resolver = Resolver::new(source);
        let errors: Vec<String> = resolver
            .resolve(&prg)
            .iter()
            .map(|err| err.to_string())
            .collect();

        print_error(&errors.join("\n\n"), writer)?;
    } else {
        report_errors(&source, &prg, writer)?;
    }

    Ok(())
}
