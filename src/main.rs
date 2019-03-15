use std::{fs, io};
use clap::{App, Arg, ArgMatches};

use kantan::{codegen::llvm::emit_to_file, compile, stdlib, Source, CompilationError};

fn main() -> Result<(), CompilationError> {
    let args = parse_args();

    let stderr = io::stderr();
    let mut err_writer = stderr.lock();

    let mut sources = args.values_of("source-file").unwrap()
        .map(|file_name| {
            (
                get_file_name(file_name),
                fs::read_to_string(&file_name)
                    .unwrap_or_else(|_| panic!("{} could not be found", file_name))
                    .replace("\t", "    "),
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
        out
    } else {
        "test.s"
    };

    emit_to_file(&mir, output_file, &mut err_writer, true);

    Ok(())
}

fn get_file_name(name: &str) -> String {
    let len = name.len();

    if name.ends_with(".kan") {
        return name.split_at(len - 4).0.to_owned();
    }

    name.to_owned()
}

pub fn parse_args<'a>() -> ArgMatches<'a> {
    App::new("kantanc")
        .version("0.1")
        .author("Felix Schoeller")
        .about("The official compiler for the Kantan programming language")
        .arg(Arg::with_name("output")
             .short("o")
             .long("output")
             .value_name("FILE")
             .help("the output file")
             .takes_value(true)
        )
        .arg(Arg::with_name("source-file")
             .multiple(true)
             .required(true)
        )
        .get_matches()
}
