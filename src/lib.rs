use std::{borrow, cmp, collections::HashMap, error, fmt, hash, io::Write};

mod cli;
#[allow(dead_code)]
mod mir;
mod parse;
mod resolve;
#[allow(dead_code)]
mod tac;
mod types;

use self::{
    mir::cfg::Cfg,
    parse::{ast::*, lexer::Lexer, parser::Parser, Span, Spanned},
    resolve::Resolver,
};

pub(crate) use self::cli::*;

#[derive(Debug)]
pub struct Source {
    pub name: String,
    pub code: String,
}

impl hash::Hash for Source {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl cmp::PartialEq for Source {
    fn eq(&self, other: &Source) -> bool {
        self.name == other.name
    }
}

impl cmp::Eq for Source {}

impl borrow::Borrow<str> for &Source {
    fn borrow(&self) -> &str {
        self.name.as_str()
    }
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

pub struct NoMainFunctionError;

impl fmt::Debug for NoMainFunctionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "No main function found")
    }
}

impl fmt::Display for NoMainFunctionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "No main function found")
    }
}

impl error::Error for NoMainFunctionError {}

pub fn compile<W: Write>(sources: &[Source], writer: &mut W) -> Result<(), Box<dyn error::Error>> {
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

    let ast_sources = sources
        .iter()
        .zip(parse_trees.iter())
        .map(|(src, prg)| (src.name.as_str(), (src, prg)))
        .collect::<HashMap<&str, (&Source, &Program)>>();

    // Try to find the main function in one of the ASTs
    let main = ast_sources
        .iter()
        .find(|(_, (_, prg))| {
            prg.0.iter().any(|top_lvl| {
                if let TopLvl::FnDecl { name, .. } = top_lvl {
                    name.node == "main"
                } else {
                    false
                }
            })
        })
        .map(|(src, _)| src);

    if main.is_none() {
        return Err(Box::new(NoMainFunctionError));
    }

    let main = main.unwrap();

    if err_count == 0 {
        // TODO: implement properly
        let types = {
            let mut resolver = Resolver::new(main, &ast_sources);
            let errors: Vec<String> = resolver
                .resolve()
                .iter()
                .map(|err| err.to_string())
                .collect();

            if !errors.is_empty() {
                print_error(&errors.join("\n\n"), writer)?;
                return Ok(());
            }

            resolver.expr_types
        };

        let (_, main_prg) = ast_sources[main];

        for top_lvl in &main_prg.0 {
            if let TopLvl::FnDecl { name, body, params } = top_lvl {
                let name = name.node;
                let body = body.0.clone();
                let params = params.0.iter().map(|Param(_, ty)| *ty).collect();
                dbg!(Cfg::function(name, params, body, &types).blocks);
            }
        }
    } else {
        for (source, ast) in ast_sources.values() {
            report_errors(source, ast, writer)?;
        }
    }

    Ok(())
}
