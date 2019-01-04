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

    mini_rust::compile(&source, &mut cursor).unwrap();
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
