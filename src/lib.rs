mod hir;
mod parse;
mod resolve;
mod types;

use self::{
    parse::{ast::*, lexer::Lexer, parser::Parser},
    resolve::Resolver,
};

pub fn compile(source: &str) {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);

    let prg = parser.parse();

    if parser.err_count == 0 {
        let mut resolver = Resolver::new();
        resolver.resolve(prg);
    } else {
        println!("{} errors found in Program", parser.err_count);
        report_errors(&prg);
    }
}

fn report_errors(prg: &Program) {
    for stmt in &prg.0 {
        if let Stmt::Expr(Expr::Error(err)) = stmt {
            println!("{}", err)
        }
    }
}
