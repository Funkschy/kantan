use std::{
    borrow::Borrow, cmp, collections::HashMap, error, fmt, hash, io, io::Write, mem, time::Instant,
};

use clap::ArgMatches;

use codegen::llvm::{emit_to_file, CodeGenArgs, CodeGenOptLevel, OutputType};

mod cli;
mod codegen;
mod mir;
mod parse;
mod resolve;
mod types;

use self::{
    mir::{func::Func, FunctionHead, Mir, MirBuilder},
    parse::{ast::*, lexer::Lexer, parser::Parser, Span, Spanned},
    resolve::{ResolveResult, Resolver},
    types::*,
};

pub(crate) use self::cli::*;
use crate::mir::ModTypeMap;
use crate::resolve::modmap::ModMap;

pub const NAME: &str = env!("CARGO_PKG_NAME");
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const REPO_URL: &str = env!("CARGO_PKG_REPOSITORY");
pub const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");
pub const DESCRIPTION: &str = env!("CARGO_PKG_DESCRIPTION");

pub struct Unit<'src> {
    source: &'src Source,
    ast: Program<'src>,
}

impl<'src> Unit<'src> {
    pub fn new(source: &'src Source, ast: Program<'src>) -> Self {
        Unit { source, ast }
    }
}

pub type PrgMap<'src> = HashMap<&'src str, Unit<'src>>;

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
pub struct FuncDef<'src> {
    name: &'src str,
    ret_type: Spanned<Type<'src>>,
    params: Vec<Spanned<Type<'src>>>,
    varargs: bool,
}

impl<'src> Default for FuncDef<'src> {
    fn default() -> Self {
        FuncDef {
            name: "",
            ret_type: Spanned::new(0, 0, Type::Simple(Simple::Void)),
            params: vec![],
            varargs: false,
        }
    }
}

pub type UserTypeMap<'src> = HashMap<&'src str, UserTypeDefinition<'src>>;
pub type FunctionMap<'src> = HashMap<&'src str, FuncDef<'src>>;
pub type MirFuncMap<'src> = HashMap<String, Func<'src>>;

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
        Self {
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

fn parse(sources: &[Source]) -> (Vec<Program>, usize) {
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
            .map(|(src, prg)| (src.name.as_str(), Unit::new(src, prg)))
            .collect(),
        err_count,
    )
}

fn find_main<'src>(ast_sources: &PrgMap<'src>) -> Option<&'src str> {
    ast_sources
        .iter()
        .find(|(_, unit)| {
            unit.ast.0.iter().any(|top_lvl| {
                if let TopLvl::FuncDecl { name, .. } = top_lvl {
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
        .map(std::string::ToString::to_string)
        .collect();

    if !errors.is_empty() {
        print_error(&errors.join("\n\n"), writer)?;
        return Err(CompilationError::TypeCheckError);
    }

    Ok(resolver.get_result())
}

fn construct_tac<'src>(
    ast_sources: &PrgMap<'src>,
    resolve_result: ResolveResult<'src>,
) -> Mir<'src> {
    let mut tac = MirBuilder::new(resolve_result);
    for (src_name, unit) in ast_sources.iter() {
        for top_lvl in &unit.ast.0 {
            if !tac.functions.contains_key(src_name) {
                // TODO: remove, when stdlib is implemented
                // since io is currently inserted into the ast manually, the mir generation would
                // crash, because it uses the resolve_result to prepare its modules and io may not
                // have been imported
                continue;
            }

            if let TopLvl::FuncDecl {
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
                    .map(|Param(n, ty)| (n.node, ty.node.clone()))
                    .collect();

                let head = FunctionHead::new(
                    name.node.to_owned(),
                    params,
                    ret_type.node.clone(),
                    *is_extern,
                    varargs,
                );

                tac.add_function(src_name, head, body);
            }
        }
    }

    // move definitions out of tac to avoid copying
    let definitions = mem::replace(&mut tac.definitions, ModMap::default());
    let mut types: ModTypeMap = HashMap::default();
    for (mod_name, typedef) in definitions.move_iter_types() {
        let module = types.entry(mod_name).or_insert_with(Vec::new);
        module.push(typedef)
    }

    Mir {
        global_strings: tac.literals,
        functions: tac.functions,
        types,
    }
}

// TODO: do properly
pub fn stdlib() -> Vec<Source> {
    let io = Source::new(
        "io",
        "extern def putchar(i: i32): i32;
         extern def puts(s: string): i32;
         extern def printf(fmt: string, ...): i32;",
    );
    vec![io]
}

pub fn compile<'src, W: Write>(
    sources: &'src [Source],
    writer: &mut W,
) -> Result<Mir<'src>, CompilationError> {
    init_ansi();
    let mut now = Instant::now();
    let (mut ast_sources, err_count) = ast_sources(sources);

    if err_count != 0 {
        for Unit { source, ast } in ast_sources.values() {
            report_errors(source, ast, writer)?;
        }
        return Err(CompilationError::ParseError);
    }
    println!("Parsing:          {} μs", now.elapsed().as_micros());

    // Try to find the main function in one of the ASTs
    let main = find_main(&ast_sources);

    if main.is_none() {
        return Err(CompilationError::NoMain(NoMainFunctionError));
    }

    let main = main.unwrap();

    now = Instant::now();
    let symbols = type_check(main, &mut ast_sources, writer)?;
    println!("Type checking:    {} μs", now.elapsed().as_micros());

    now = Instant::now();
    let mir = construct_tac(&ast_sources, symbols);
    println!("Mir construction: {} μs", now.elapsed().as_micros());

    Ok(mir)
}

pub fn llvm_emit_to_file<W: Write>(mir: &Mir, err_writer: &mut W, args: &ArgMatches, dump: bool) {
    let output_file = if let Some(out) = args.value_of("output") {
        out.trim()
    } else {
        "out.o"
    };

    let opt_lvl = args
        .value_of("opt")
        .and_then(CodeGenOptLevel::convert)
        .unwrap_or(CodeGenOptLevel::None);

    let output_type = args
        .value_of("emit")
        .and_then(|ty| OutputType::convert(ty.trim()))
        .unwrap_or(OutputType::Object);

    let mut codegen_args = CodeGenArgs::new(output_file, err_writer, output_type, opt_lvl);
    let now = Instant::now();
    emit_to_file(&mir, &mut codegen_args, dump);
    println!("LLVM compilation: {} μs", now.elapsed().as_micros());
}
