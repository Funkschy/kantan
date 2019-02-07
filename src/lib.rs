use std::{borrow, cmp, collections::HashMap, error, fmt, hash, io, io::Write};

mod cli;
#[allow(dead_code)]
mod codegen;
#[allow(dead_code)]
mod mir;
mod parse;
mod resolve;
mod types;

use self::{
    mir::Tac,
    parse::{ast::*, lexer::Lexer, parser::Parser, Span, Spanned},
    resolve::{Resolver, TypeMap},
};

pub(crate) use self::cli::*;

pub type PrgMap<'input> = HashMap<&'input str, (&'input Source, Program<'input>)>;

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

#[derive(Debug)]
pub enum CompilationError {
    NoMain(NoMainFunctionError),
    ParseError,
    TypeCheckError,
    IO(io::Error),
}

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use CompilationError::*;

        match self {
            NoMain(err) => write!(f, "{}", err),
            ParseError => write!(f, "Error while parsing"),
            TypeCheckError => write!(f, "Error while typechecking"),
            IO(err) => write!(f, "{}", err),
        }
    }
}

impl error::Error for CompilationError {}

impl From<io::Error> for CompilationError {
    fn from(err: io::Error) -> Self {
        CompilationError::IO(err)
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

fn init_ansi() {
    #[cfg(windows)]
    {
        if let Err(code) = ansi_term::enable_ansi_support() {
            eprintln!(
                "Could not initialise windows ansi support. Error code: {}",
                code
            );
        }
    }
}

fn parse<'input>(sources: &'input [Source]) -> (Vec<Program<'input>>, usize) {
    sources
        .iter()
        .fold((vec![], 0), |(mut asts, err_count), source| {
            let lexer = Lexer::new(&source.code);
            let mut parser = Parser::new(lexer);
            let prg = parser.parse();
            asts.push(prg);
            (asts, err_count + parser.err_count)
        })
}

fn ast_sources<'input, 'ast>(sources: &'input [Source]) -> (PrgMap<'input>, usize) {
    let (parse_trees, err_count) = parse(sources);
    (
        sources
            .iter()
            .zip(parse_trees.into_iter())
            .map(|(src, prg)| (src.name.as_str(), (src, prg)))
            .collect(),
        err_count,
    )
}

fn find_main<'input, 'ast>(ast_sources: &PrgMap<'input>) -> Option<&'input str> {
    ast_sources
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
        .map(|(src, _)| *src)
}

fn type_check<'input, 'ast, W: Write>(
    main: &'input str,
    ast_sources: &'input PrgMap<'input>,
    writer: &mut W,
) -> Result<TypeMap<'input, 'ast>, CompilationError> {
    let mut resolver = Resolver::new(main, &ast_sources);
    let errors: Vec<String> = resolver
        .resolve()
        .iter()
        .map(|err| err.to_string())
        .collect();

    if !errors.is_empty() {
        print_error(&errors.join("\n\n"), writer)?;
        return Err(CompilationError::TypeCheckError);
    }

    Ok(resolver.expr_types)
}

fn tac_functions<'input, 'ast>(
    types: &'input TypeMap<'input, 'ast>,
    ast_sources: &PrgMap<'input>,
) -> Tac<'input, 'ast> {
    let mut tac = Tac::new(&types);
    for (_, (_, prg)) in ast_sources.iter() {
        for top_lvl in &prg.0 {
            if let TopLvl::FnDecl {
                name,
                body,
                params,
                ret_type,
            } = top_lvl
            {
                let name = name.node;
                let params = params.0.iter().map(|Param(n, ty)| (n.node, *ty)).collect();
                let ret_type = ret_type.node;

                tac.add_function(name.to_owned(), params, &body, ret_type);
            }
        }
    }
    tac
}

pub fn compile<W: Write>(sources: &[Source], writer: &mut W) -> Result<(), CompilationError> {
    init_ansi();
    let (ast_sources, err_count) = ast_sources(sources);

    if err_count != 0 {
        for (source, ast) in ast_sources.values() {
            report_errors(source, ast, writer)?;
        }
        return Err(CompilationError::ParseError);
    }

    // Try to find the main function in one of the ASTs
    // TODO: require one of the files to be called main.*
    let main = find_main(&ast_sources);

    if main.is_none() {
        return Err(CompilationError::NoMain(NoMainFunctionError));
    }

    let main = main.unwrap();

    let types = type_check(main, &ast_sources, writer)?;
    let tac = tac_functions(&types, &ast_sources);

    let funcs = tac
        .functions
        .iter()
        .map(|f| f.to_string())
        .collect::<Vec<String>>()
        .join("\n");

    println!("{}", funcs);
    Ok(())
}
