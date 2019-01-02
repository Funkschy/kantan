use ansi_term::Colour::{Red, Yellow};

mod parse;
mod resolve;
mod types;

use self::{
    parse::{ast::*, lexer::Lexer, parser::Parser, Span, Spanned},
    resolve::Resolver,
};

pub fn compile(source: &str) {
    #[cfg(windows)]
    {
        if let Err(code) = ansi_term::enable_ansi_support() {
            eprintln!(
                "Could not initialise windows ansi support. Error code: {}",
                code
            );
        }
    }

    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);

    let prg = parser.parse();

    if parser.err_count == 0 {
        let mut resolver = Resolver::new(source);
        let errors = resolver.resolve(prg);

        print_error(&errors.join("\n\n"));
    } else {
        report_errors(&source, &prg);
    }
}

fn print_error(msg: &str) {
    eprintln!("{}", msg);
}

fn report_errors(source: &str, prg: &Program) {
    for stmt in &prg.0 {
        if let Stmt::Expr(Spanned {
            node: Expr::Error(err),
            span,
        }) = stmt
        {
            print_error(&format_error(&source, *span, *span, &err.to_string()));
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
        err_to_string(source, expr_span, err_tok_span, line_nr, index, false)
    )
}

fn find_line_index(source: &str, start: usize) -> (usize, usize) {
    let iter = source.char_indices().rev().skip(source.len() - start);
    let line_nr = iter.clone().filter(|(_, c)| *c == '\n').count() + 1;

    // For some reason, the index on windows is exactly 4 units shorter, than it should be
    let index = {
        let value = iter.take_while(|(_, c)| *c != '\n').count() + 1;

        #[cfg(windows)]
        {
            value + 4
        }

        #[cfg(not(windows))]
        {
            value
        }
    };

    (line_nr, index)
}

fn err_to_string(
    source: &str,
    expr_span: Span,
    err_tok_span: Span,
    line_nr: usize,
    index: usize,
    warning: bool,
) -> String {
    let (start_line, _) = find_line_index(source, expr_span.start);
    let (end_line, _) = find_line_index(source, expr_span.end);

    let start_line = start_line - 1;

    // the number of digits in the number displayed as string
    let len_line_nr = (line_nr / 10) + 1;
    let filler = " ".repeat(len_line_nr + 1);

    let len = err_tok_span.end - err_tok_span.start + 1;
    let marker = format!("{}{}", " ".repeat(index - 1), "^".repeat(len));
    let marker = if warning {
        Yellow.paint(marker)
    } else {
        Red.paint(marker)
    };

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
