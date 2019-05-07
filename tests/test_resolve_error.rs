use std::io::Cursor;

use kantan::*;

#[test]
fn test_invalid_assignment() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test",
        r#"def main(): void {
    let mystr = "hello";
    mystr = 5;
}"#,
    );

    kantan::compile(&[source], &mut cursor).unwrap_err();
    let output = String::from_utf8(cursor.into_inner()).unwrap();

    assert_eq!(
        "error: binary operation '=' cannot be applied to 'string' and 'i32'
--> test:3:11
  |
3 |    mystr = 5;
  |\u{1b}[31m          ^\u{1b}[0m - not allowed

reason:
  |
2 |    let mystr = \"hello\";
  |\u{1b}[33m        ^^^^^\u{1b}[0m - 'mystr' was defined as 'string' here
",
        output
    );
}

#[test]
fn test_invalid_assignment_explicit_type() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test",
        r#"def main(): void {
    let mystr: string = 2;
}"#,
    );

    kantan::compile(&[source], &mut cursor).unwrap_err();
    let output = String::from_utf8(cursor.into_inner()).unwrap();

    assert_eq!(
        "error: binary operation '=' cannot be applied to 'string' and 'i32'
--> test:2:23
  |
2 |    let mystr: string = 2;
  |\u{1b}[31m                      ^\u{1b}[0m - not allowed

reason:
  |
2 |    let mystr: string = 2;
  |\u{1b}[33m        ^^^^^\u{1b}[0m - 'mystr' was defined as 'string' here
",
        output
    );
}

#[test]
fn test_non_bool_in_if_condition() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test",
        r#"def main(): void {
    if "hello" {

    }
}"#,
    );

    kantan::compile(&[source], &mut cursor).unwrap_err();
    let output = String::from_utf8(cursor.into_inner()).unwrap();
    let expected = "error: if condition must be of type 'bool', but the supplied type was 'string'
--> test:2:9
  |
2 |    if \"hello\" {
  |\u{1b}[31m        ^^^^^\u{1b}[0m
";
    assert_eq!(expected, output);
}

#[test]
fn test_call_of_undefined_function() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test",
        r#"def main(): void {
    test();
}
"#,
    );

    kantan::compile(&[source], &mut cursor).unwrap_err();
    let output = String::from_utf8(cursor.into_inner()).unwrap();
    let expected = "error: 'test' not in scope
--> test:2:5
  |
2 |    test();
  |\u{1b}[31m    ^^^^\u{1b}[0m
";
    assert_eq!(expected, output);
}

#[test]
fn test_add_string_should_return_type_error() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test-equals",
        r#"def main(): void {
 let s = "hello " + "world";
 }"#,
    );

    kantan::compile(&[source], &mut cursor).unwrap_err();
    let output = String::from_utf8(cursor.into_inner()).unwrap();

    let expected = concat!(
        r#"error: binary operation '+' cannot be applied to 'string' and 'string'
--> test-equals:2:19
  |
2 | let s = "hello " + "world";
  |"#,
        "\u{1b}[31m                  ^\u{1b}[0m - not allowed
"
    );

    assert_eq!(expected, output);
}

#[test]
fn test_invalid_equality_operation_should_return_error_message() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test-equals",
        r#"def main(): void {
 let s = "hello";
 let s2 = 2;
 let res = s == s2;
 }"#,
    );

    kantan::compile(&[source], &mut cursor).unwrap_err();
    let output = String::from_utf8(cursor.into_inner()).unwrap();

    let expected = "error: binary operation '==' cannot be applied to 'string' and 'i32'
--> test-equals:4:14
  |
4 | let res = s == s2;
  |\u{1b}[31m             ^^\u{1b}[0m - not allowed
";

    assert_eq!(expected, output);
}

#[test]
fn test_call_of_non_function_type_should_return_error_message() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test-call-non-func",
        r#"def main(): void {
"test"();
 }"#,
    );

    kantan::compile(&[source], &mut cursor).unwrap_err();
    let output = String::from_utf8(cursor.into_inner()).unwrap();

    let expected = concat!(
        r#"error: trying to call variable of type 'string', but UFCS is not supported yet
--> test-call-non-func:2:2
  |
2 |"test"();
"#,
        "  |\u{1b}[31m ^^^^\u{1b}[0m
"
    );

    assert_eq!(expected, output);
}
