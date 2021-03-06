use std::io::{self, Write};

use super::{parse::ast::*, Source, Span, Spanned};

use ansi_term::Colour::{Red, Yellow};
use unicode_width::UnicodeWidthStr;

pub fn print_error<W: Write>(msg: &str, writer: &mut W) -> io::Result<()> {
    writer.write_all(msg.as_bytes())?;
    writer.write_all(b"\n")?;
    writer.flush()?;
    Ok(())
}

pub fn report_errors<W: Write>(source: &Source, prg: &Program, writer: &mut W) -> io::Result<()> {
    for (span, msg) in find_errors(prg) {
        print_error(&format_error(source, span, span, &msg), writer)?;
    }

    Ok(())
}

fn find_errors(prg: &Program) -> Vec<(Span, String)> {
    fn find_errors_rec(stmt: &Stmt, errors: &mut Vec<(Span, String)>) {
        match stmt {
            Stmt::VarDecl(decl) => {
                if decl.value.node.is_err() {
                    errors.push((decl.value.span, decl.value.node.to_string()))
                }
            }
            Stmt::Expr(Spanned { node: expr, span }) => {
                if expr.is_err() {
                    errors.push((*span, expr.to_string()));
                }
            }
            Stmt::Delete(expr) => {
                if expr.node.is_err() {
                    errors.push((expr.span, expr.node.to_string()));
                }
            }
            Stmt::Return(val) => {
                if let Some(Spanned { node: expr, span }) = val {
                    if expr.is_err() {
                        errors.push((*span, expr.to_string()));
                    }
                }
            }
            Stmt::While(while_stmt) => {
                let WhileStmt {
                    condition:
                        Spanned {
                            node: condition,
                            span,
                        },
                    body,
                } = while_stmt.as_ref();

                if condition.is_err() {
                    errors.push((*span, condition.to_string()));
                }

                for s in &body.0 {
                    find_errors_rec(s, errors);
                }
            }
            Stmt::If(if_stmt) => {
                let IfStmt {
                    condition:
                        Spanned {
                            node: condition,
                            span,
                        },
                    then_block,
                    else_branch,
                } = if_stmt.as_ref();
                if condition.is_err() {
                    errors.push((*span, condition.to_string()));
                }

                for s in &then_block.0 {
                    find_errors_rec(s, errors);
                }

                if let Some(else_branch) = else_branch {
                    match else_branch.as_ref() {
                        Else::IfStmt(s) => find_errors_rec(s, errors),
                        Else::Block(b) => {
                            for s in &b.0 {
                                find_errors_rec(s, errors);
                            }
                        }
                    }
                }
            }
        }
    }

    let mut errors = vec![];
    for top_lvl in &prg.0 {
        match top_lvl {
            TopLvl::FuncDecl { body, .. } => {
                for s in &body.0 {
                    find_errors_rec(s, &mut errors);
                }
            }
            TopLvl::Error(err) => {
                errors.push((err.span, err.node.to_string()));
            }
            TopLvl::TypeDef(..) | TopLvl::Import { .. } => {}
        }
    }
    errors
}

pub fn format_error(source: &Source, expr_span: Span, err_tok_span: Span, msg: &str) -> String {
    let (line_nr, index) = find_line_index(source, err_tok_span.start);

    format!(
        "error: {}\n--> {}:{}:{}\n{}",
        msg,
        source.name,
        line_nr,
        index,
        err_to_string(source, expr_span, err_tok_span, line_nr, false)
    )
}

pub fn find_line_index(source: &Source, start: usize) -> (usize, usize) {
    let slice = &source.code[..start];

    let line_nr = slice.chars().filter(|c| *c == '\n').count() + 1;
    let index = slice.chars().rev().take_while(|c| *c != '\n').count() + 1;

    (line_nr, index)
}

fn find_dist(source: &Source, start: usize) -> usize {
    let slice = &source.code[..start];
    let slice = slice
        .chars()
        .rev()
        .take_while(|c| *c != '\n')
        .collect::<String>();

    // tabs are replaced by 4 spaces in the output, so add those to the distance
    let tabs = slice.chars().filter(|c| *c == '\t').count() * 4;

    UnicodeWidthStr::width(slice.as_str()) + tabs
}

pub fn err_to_string(
    source: &Source,
    expr_span: Span,
    err_tok_span: Span,
    line_nr: usize,
    warning: bool,
) -> String {
    let (start_line, _) = find_line_index(source, expr_span.start);
    let (end_line, _) = find_line_index(source, expr_span.end);

    let start_line = start_line - 1;

    // the number of digits in the number displayed as string
    let len_line_nr = line_nr.to_string().len();
    let filler = " ".repeat(len_line_nr + 1);

    let slice = &source.code[err_tok_span.start..err_tok_span.end];
    let len = UnicodeWidthStr::width(slice) + 1;
    let dist = find_dist(source, err_tok_span.start);

    let marker = format!("{}{}", " ".repeat(dist), "^".repeat(len));
    let marker = if warning {
        Yellow.paint(marker)
    } else {
        Red.paint(marker)
    };

    let lines: Vec<String> = source
        .code
        .lines()
        .enumerate()
        .skip(start_line)
        .take(end_line - start_line)
        .map(|(nr, l)| (nr, l.replace("\t", "    ")))
        .map(|(nr, l)| {
            if nr + 1 == line_nr {
                format!("{}|\n{} |{}\n{}|{}", filler, line_nr, l, filler, marker)
            } else {
                format!("{}|{}", filler, l)
            }
        })
        .collect();

    lines.join("\n")
}
