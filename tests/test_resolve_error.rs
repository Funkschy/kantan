use std::io::Cursor;

use mini_rust::*;

#[test]
fn test_invalid_assignment() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test",
        r#"fn main() {
    let mystr = "hello";
    mystr = 5;
}"#,
    );

    mini_rust::compile(&vec![source], &mut cursor).unwrap();
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
        r#"fn main() {
    let mystr: string = 2;
}"#,
    );

    mini_rust::compile(&vec![source], &mut cursor).unwrap();
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
        r#"fn main() {
    if "hello" {

    }
}"#,
    );

    mini_rust::compile(&vec![source], &mut cursor).unwrap();
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
        r#"fn main() {
    test();
}
"#,
    );

    mini_rust::compile(&vec![source], &mut cursor).unwrap();
    let output = String::from_utf8(cursor.into_inner()).unwrap();
    let expected = "error: 'test' not in scope
--> test:2:5
  |
2 |    test();
  |\u{1b}[31m    ^^^^\u{1b}[0m
";
    assert_eq!(expected, output);
}