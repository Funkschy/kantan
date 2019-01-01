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
        let errors = resolver.resolve(prg);

        println!("{}", errors.join("\n\n"));
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

fn format_error(source: &str, expr_span: Span, err_tok_span: Span, msg: &str) -> String {
    let (line_nr, index) = find_line_index(source, err_tok_span.start);

    format!(
        "error: {}\n--> {}:{}\n{}",
        msg,
        line_nr,
        index,
        err_to_string(source, expr_span, err_tok_span, line_nr, index)
    )
}

fn find_line_index(source: &str, start: usize) -> (usize, usize) {
    let iter = source.char_indices().rev().skip(source.len() - start);
    let line_nr = iter.clone().filter(|(_, c)| *c == '\n').count() + 1;
    let index = iter.take_while(|(_, c)| *c != '\n').count() + 1;

    (line_nr, index)
}

fn err_to_string(
    source: &str,
    expr_span: Span,
    err_tok_span: Span,
    line_nr: usize,
    index: usize,
) -> String {
    let (start_line, _) = find_line_index(source, expr_span.start);
    let (end_line, _) = find_line_index(source, expr_span.end);

    let start_line = start_line - 1;

    // the number of digits in the number displayed as string
    let len_line_nr = (line_nr / 10) + 1;
    let filler = " ".repeat(len_line_nr + 1);

    let len = err_tok_span.end - err_tok_span.start + 1;
    let marker = format!("{}{}", " ".repeat(index - 1), "^".repeat(len));

    let lines: Vec<String> = source
        .lines()
        .enumerate()
        .skip(start_line)
        .take(end_line - start_line)
        .map(|(nr, l)| {
            if nr + 1 == line_nr {
                format!("{} |{}\n{}|{}", line_nr, l, filler, marker)
            } else {
                format!("{}|{}", filler, l)
            }
        })
        .collect();

    lines.join("\n")
}
