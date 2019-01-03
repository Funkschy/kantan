use std::io::{self, Write};

use ansi_term::Colour::{Red, Yellow};
use unicode_width::UnicodeWidthStr;

mod parse;
mod resolve;
mod types;

use self::{
    parse::{ast::*, lexer::Lexer, parser::Parser, Span, Spanned},
    resolve::Resolver,
};

pub fn compile<W: Write>(source: &str, writer: &mut W) -> io::Result<()> {
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

        print_error(&errors.join("\n\n"), writer)?;
    } else {
        print_error(
            &format!("{} error(s) while parsing", parser.err_count),
            writer,
        )?;
        report_errors(&source, &prg, writer)?;
    }

    Ok(())
}

fn print_error<W: Write>(msg: &str, writer: &mut W) -> io::Result<()> {
    writer.write_all(msg.as_bytes())?;
    writer.write(b"\n")?;
    writer.flush()?;
    Ok(())
}

fn report_errors<W: Write>(source: &str, prg: &Program, writer: &mut W) -> io::Result<()> {
    for (span, msg) in find_errors(prg) {
        print_error(&format_error(&source, span, span, &msg), writer)?;
    }

    Ok(())
}

fn find_errors(prg: &Program) -> Vec<(Span, String)> {
    fn find_errors_rec(stmt: &Stmt, errors: &mut Vec<(Span, String)>) {
        match stmt {
            Stmt::VarDecl { value, .. } => {
                if let Spanned {
                    node: Expr::Error(err),
                    span,
                } = value
                {
                    errors.push((*span, err.to_string()))
                }
            }
            Stmt::FnDecl { body, .. } => {
                for s in &body.0 {
                    find_errors_rec(s, errors);
                }
            }
            Stmt::Expr(Spanned { node: expr, span }) => {
                if let Expr::Error(err) = expr {
                    errors.push((*span, err.to_string()));
                }
            }
        }
    }

    let mut errors = vec![];
    for s in &prg.0 {
        find_errors_rec(s, &mut errors);
    }
    errors
}

fn format_error(source: &str, expr_span: Span, err_tok_span: Span, msg: &str) -> String {
    let (line_nr, index) = find_line_index(source, err_tok_span.start);

    format!(
        "error: {}\n--> {}:{}\n{}",
        msg,
        line_nr,
        index,
        err_to_string(source, expr_span, err_tok_span, line_nr, false)
    )
}

fn find_line_index(source: &str, start: usize) -> (usize, usize) {
    let slice = &source[..start];

    let line_nr = slice.chars().filter(|c| *c == '\n').count() + 1;
    let index = slice.chars().rev().take_while(|c| *c != '\n').count() + 1;

    (line_nr, index)
}

fn find_dist(source: &str, start: usize) -> usize {
    let slice = &source[..start];

    UnicodeWidthStr::width(
        slice
            .chars()
            .rev()
            .take_while(|c| *c != '\n')
            .collect::<String>()
            .as_str(),
    )
}

fn err_to_string(
    source: &str,
    expr_span: Span,
    err_tok_span: Span,
    line_nr: usize,
    warning: bool,
) -> String {
    let (start_line, _) = find_line_index(source, expr_span.start);
    let (end_line, _) = find_line_index(source, expr_span.end);

    let start_line = start_line - 1;

    // the number of digits in the number displayed as string
    let len_line_nr = (line_nr / 10) + 1;
    let filler = " ".repeat(len_line_nr + 1);

    // let len = err_tok_span.end - err_tok_span.start + 1;
    let len = UnicodeWidthStr::width(&source[err_tok_span.start..err_tok_span.end]) + 1;
    let dist = find_dist(source, err_tok_span.start);

    let marker = format!("{}{}", " ".repeat(dist), "^".repeat(len));
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
