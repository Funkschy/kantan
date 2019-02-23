use std::{env, error, fs, io};

use kantan::{codegen::llvm::emit_to_file, compile, Source};

fn main() -> Result<(), Box<dyn error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_help();
        return Ok(());
    }

    let stderr = io::stderr();
    let mut err_writer = stderr.lock();

    let sources = args
        .iter()
        .skip(1)
        .map(|file_name| {
            (
                file_name,
                fs::read_to_string(file_name).unwrap().replace("\t", "    "),
            )
        })
        .map(|(file_name, code)| Source {
            name: file_name.to_owned(),
            code,
        })
        .collect::<Vec<Source>>();

    let tac_functions = compile(&sources, &mut err_writer)?;

    let funcs = tac_functions
        .iter()
        .map(|f| f.to_string())
        .collect::<Vec<String>>()
        .join("\n");

    println!("{}", funcs);

    emit_to_file(&tac_functions, "target/test.s", &mut err_writer);

    Ok(())
}

fn print_help() {
    println!("Usage: mini-rust FILE_NAMES");
}
