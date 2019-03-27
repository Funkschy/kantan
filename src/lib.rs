use std::{borrow::Borrow, cmp, collections::HashMap, error, fmt, hash, io, io::Write};

mod cli;
pub mod codegen;
mod mir;
mod parse;
mod resolve;
mod types;

use self::{
    mir::{func::Func, tac::Label, Tac},
    parse::{ast::*, lexer::Lexer, parser::Parser, Span, Spanned},
    resolve::{ModTypeMap, ResolveResult, Resolver},
    types::*,
};

pub(crate) use self::cli::*;

pub type PrgMap<'src> = HashMap<&'src str, (&'src Source, Program<'src>)>;

#[derive(Debug)]
pub struct UserTypeDefinition<'src> {
    pub name: &'src str,
    pub fields: HashMap<&'src str, (u32, Spanned<Type<'src>>)>,
}

impl<'src> fmt::Display for UserTypeDefinition<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let fields = self
            .fields
            .iter()
            .map(|(name, (_, Spanned { node, .. }))| format!("    {}: {}", name, node))
            .collect::<Vec<String>>()
            .join(",\n");

        write!(f, "type {} struct {{\n{}\n}}", self.name, fields)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition<'src> {
    ret_type: Spanned<Type<'src>>,
    params: Vec<Spanned<Type<'src>>>,
    varargs: bool,
}

pub type UserTypeMap<'src> = HashMap<&'src str, UserTypeDefinition<'src>>;
pub type FunctionMap<'src> = HashMap<&'src str, FunctionDefinition<'src>>;
pub type MirFuncMap<'src> = HashMap<&'src str, Func<'src>>;

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

impl Borrow<str> for &Source {
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

fn parse<'src>(sources: &'src [Source]) -> (Vec<Program<'src>>, usize) {
    sources
        .iter()
        .fold((vec![], 0), |(mut asts, err_count), source| {
            let lexer = Lexer::new(&source);
            let mut parser = Parser::new(lexer);
            let prg = parser.parse();
            asts.push(prg);
            (asts, err_count + parser.err_count)
        })
}

fn ast_sources(sources: &[Source]) -> (PrgMap<'_>, usize) {
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

fn find_main<'src>(ast_sources: &PrgMap<'src>) -> Option<&'src str> {
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

fn type_check<'src, W: Write>(
    main: &'src str,
    ast_sources: &mut PrgMap<'src>,
    writer: &mut W,
) -> Result<ResolveResult<'src>, CompilationError> {
    let mut resolver = Resolver::new(main, ast_sources);
    let errors: Vec<String> = resolver
        .resolve()
        .iter()
        .map(|err| err.to_string())
        .collect();

    if !errors.is_empty() {
        print_error(&errors.join("\n\n"), writer)?;
        return Err(CompilationError::TypeCheckError);
    }

    Ok(resolver.get_result())
}

// TODO: move to mir module
#[derive(Debug)]
pub struct Mir<'src> {
    pub global_strings: HashMap<Label, &'src str>,
    pub functions: HashMap<&'src str, MirFuncMap<'src>>,
    pub types: ModTypeMap<'src>,
}

impl<'src> fmt::Display for Mir<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let types = self
            .types
            .iter()
            .flat_map(|(m, types)| {
                types
                    .iter()
                    .map(|(_, v)| format!("{}.{}", m, v))
                    .collect::<Vec<String>>()
            })
            .collect::<Vec<String>>()
            .join("\n");

        let global_strings = self
            .global_strings
            .iter()
            .map(|(k, v)| format!("{} {}", k, v))
            .collect::<Vec<String>>()
            .join("\n");

        let funcs = self
            .functions
            .iter()
            .flat_map(|(m, funcs)| {
                funcs
                    .iter()
                    .map(|(_, v)| format!("{}.{}", m, v))
                    .collect::<Vec<String>>()
            })
            .collect::<Vec<String>>()
            .join("\n\n");

        write!(f, "{}\n{}\n{}", types, global_strings, funcs)
    }
}

fn construct_tac<'src>(
    ast_sources: &PrgMap<'src>,
    resolve_result: ResolveResult<'src>,
) -> Mir<'src> {
    let mut tac = Tac::new(resolve_result);
    for (src_name, (_, prg)) in ast_sources.iter() {
        for top_lvl in prg.0.iter() {
            if !tac.functions.contains_key(src_name) {
                // since io is currently inserted into the ast manually, the mir generation would
                // crash, because it uses the resolve_result to prepare its modules and io may not
                // have been imported
                continue;
            }

            if let TopLvl::FnDecl {
                name,
                body,
                params,
                ret_type,
                is_extern,
            } = top_lvl
            {
                let varargs = params.varargs;

                let params = params
                    .params
                    .iter()
                    .map(|Param(n, ty)| (n.node, ty.node))
                    .collect();
                let ret_type = ret_type.node;

                let ident = UserIdent::new(src_name, name.node);

                tac.add_function(ident, params, &body, ret_type, *is_extern, varargs);
            }
        }
    }
    Mir {
        global_strings: tac.literals,
        functions: tac.functions,
        types: tac.types,
    }
}

// TODO: do properly
pub fn stdlib() -> Vec<Source> {
    let io = Source::new(
        "io",
        "extern fn putchar(i: i32): i32; 
         extern fn puts(s: string): i32;
         extern fn printf(fmt: string, ...): i32;",
    );
    vec![io]
}

pub fn compile<'src, W: Write>(
    sources: &'src [Source],
    writer: &mut W,
) -> Result<Mir<'src>, CompilationError> {
    init_ansi();
    println!("Parsing...");
    let (mut ast_sources, err_count) = ast_sources(sources);

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
    println!("Type checking...");
    let symbols = type_check(main, &mut ast_sources, writer)?;

    println!("Constructing mir...");
    let mir = construct_tac(&ast_sources, symbols);

    Ok(mir)
}
