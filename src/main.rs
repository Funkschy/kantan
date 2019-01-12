use std::{env, fs, io};

use mini_rust::{compile, Source};

fn main() -> io::Result<()> {
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

    compile(&sources, &mut err_writer)?;
    Ok(())
}

fn print_help() {
    println!("Usage: mini-rust FILE_NAMES");
}
