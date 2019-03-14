use std::{env, error, fs, io};

use kantan::{codegen::llvm::emit_to_file, compile, stdlib, Source};

fn main() -> Result<(), Box<dyn error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_help();
        return Ok(());
    }

    let stderr = io::stderr();
    let mut err_writer = stderr.lock();

    let mut sources = args
        .iter()
        .skip(1)
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

    emit_to_file(&mir, "test.s", &mut err_writer, true);

    Ok(())
}

fn get_file_name(name: &str) -> String {
    let len = name.len();

    if name.ends_with(".kan") {
        return name.split_at(len - 4).0.to_owned();
    }

    name.to_owned()
}

fn print_help() {
    println!("Usage: mini-rust FILE_NAMES");
}
