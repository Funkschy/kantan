mod hir;
mod parse;
mod resolve;
mod types;

use self::{
    parse::{ast::*, lexer::Lexer, parser::Parser, Span, Spanned},
    resolve::Resolver,
};

pub fn compile(source: &str) {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);

    let prg = parser.parse();

    if parser.err_count == 0 {
        let mut resolver = Resolver::new(source);
        resolver.resolve(prg);
    } else {
        println!("{} errors found in Program", parser.err_count);
        report_errors(&prg);
    }
}

fn report_errors(prg: &Program) {
    for stmt in &prg.0 {
        if let Stmt::Expr(Spanned {
            node: Expr::Error(err),
            ..
        }) = stmt
        {
            println!("{}", err)
        }
    }
}

fn format_error(source: &str, span: Span, msg: &str) -> String {
    let (line_nr, index) = find_line(source, span.start);
    format!(
        "error: {}\n--> {}:{}\n{}",
        msg,
        line_nr,
        index,
        err_to_string(source, span, line_nr, index)
    )
}

/// Returns the line number and the char index of the first char in the line
fn find_line(source: &str, start: usize) -> (usize, usize) {
    let mut iter = source
        .char_indices()
        .rev()
        .skip(source.len() - start)
        .filter(|(_, c)| *c == '\n');

    let line_nr = iter.clone().count() + 1;
    let index = start - iter.next().map(|(i, _)| i).unwrap_or(0);

    (line_nr, index)
}

fn err_to_string(source: &str, span: Span, line_nr: usize, index: usize) -> String {
    let mut counter = 1;
    let iter = source.char_indices().skip_while(|(_, c)| {
        if *c == '\n' {
            counter += 1;
        }
        counter < line_nr
    });

    let s: String = iter
        .skip(1)
        .take_while(|(i, _)| *i <= span.end)
        .map(|(_, c)| c)
        .collect();

    let len = span.end - span.start + 1;
    let marker = "^".repeat(len);
    // 1 comes from "|"
    let dist = " ".repeat(1 + index);

    s.lines()
        .map(|l| format!(" |{}\n{}{}", l, dist, marker))
        .collect()
}
