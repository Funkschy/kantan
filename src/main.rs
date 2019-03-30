use clap::{App, Arg, ArgMatches};
use std::{fs, io};

use kantan::{
    codegen::llvm::{emit_to_file, CodeGenArgs, CodeGenOptLevel, OutputType},
    compile, stdlib, CompilationError, Source, AUTHOR, DESCRIPTION, NAME, VERSION,
};

fn main() -> Result<(), CompilationError> {
    let args = parse_args();

    let stderr = io::stderr();
    let mut err_writer = stderr.lock();

    let mut sources = args
        .values_of("source-file")
        .unwrap()
        .map(|file_name| {
            (
                get_file_name(file_name),
                fs::read_to_string(&file_name)
                    .unwrap_or_else(|_| panic!("{} could not be found", file_name)),
            )
        })
        .map(|(file_name, code)| Source {
            name: file_name,
            code,
        })
        .collect::<Vec<Source>>();

    let mut stdlib = stdlib();
    sources.append(&mut stdlib);

    let mir = compile(&sources, &mut err_writer)?;
    println!("{}", mir);

    let output_file = if let Some(out) = args.value_of("output") {
        out.trim()
    } else {
        "out.o"
    };

    let opt_lvl = args
        .value_of("opt")
        .and_then(CodeGenOptLevel::convert)
        .unwrap_or(CodeGenOptLevel::OptNone);

    let output_type = args
        .value_of("emit")
        .and_then(|ty| OutputType::convert(ty.trim()))
        .unwrap_or(OutputType::Object);

    let codegen_args = CodeGenArgs::new(output_file, &mut err_writer, output_type, opt_lvl);

    emit_to_file(&mir, codegen_args);

    Ok(())
}

/// removes the .kan ending
fn get_file_name(name: &str) -> String {
    let len = name.len();

    if name.ends_with(".kan") {
        return name.split_at(len - 4).0.to_owned();
    }

    name.to_owned()
}

pub fn parse_args<'a>() -> ArgMatches<'a> {
    App::new(NAME)
        .version(VERSION)
        .author(AUTHOR)
        .about(DESCRIPTION)
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .value_name("file")
                .help("the output file")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("emit")
                .short("e")
                .long("emit")
                .value_name("type")
                .possible_values(&["asm", "obj", "llvm-ir"])
                .takes_value(true),
        )
        .arg(
            Arg::with_name("opt")
                .short("O")
                .long("opt-lvl")
                .value_name("level")
                .possible_values(&["0", "1", "2", "3"])
                .takes_value(true),
        )
        .arg(Arg::with_name("source-file").multiple(true).required(true))
        .get_matches()
}
