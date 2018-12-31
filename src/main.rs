use std::env;
use std::fs;

use mini_rust::compile;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        print_help();
        return;
    }

    let source = fs::read_to_string(&args[1]).unwrap();
    compile(&source);
}

fn print_help() {
    println!("Usage: mini-rust FILE_NAME");
}
