use std::{env, fs, io};

use mini_rust::compile;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        print_help();
        return Ok(());
    }

    let stderr = io::stderr();
    let mut err_writer = stderr.lock();

    let source = fs::read_to_string(&args[1]).unwrap();
    compile(&source, &mut err_writer)?;
    Ok(())
}

fn print_help() {
    println!("Usage: mini-rust FILE_NAME");
}
