use std::io::Cursor;

use mini_rust::*;

#[test]
fn test_multiple_plus_and_star_operator() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test",
        r#"fn main(): void {
    let mystr = "hello";
    mystr = "world";
    mystr = 1;
    let x = 1;
    x + + 2;
    x * * 2;
    y = 1;
}"#,
    );

    mini_rust::compile(&vec![source], &mut cursor).unwrap();
    let output = String::from_utf8(cursor.into_inner()).unwrap();

    assert_eq!(
        "error: Invalid token in prefix rule: '+'
--> test:6:9
  |
6 |    x + + 2;
  |\u{1b}[31m        ^\u{1b}[0m
error: Invalid token in prefix rule: \'*\'
--> test:7:9
  |
7 |    x * * 2;
  |\u{1b}[31m        ^\u{1b}[0m
",
        output
    );
}

#[test]
fn test_hiragana_identifier() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test-kana",
        r#"fn main(): void {
    let こんにちは = "hello"; 
}"#,
    );

    mini_rust::compile(&vec![source], &mut cursor).unwrap();
    let output = String::from_utf8(cursor.into_inner()).unwrap();

    assert_eq!(
        "error: Failed to lex token, because: Non ascii identifiers are currently not supported
--> test-kana:2:9
  |
2 |    let こんにちは = \"hello\"; 
  |\u{1b}[31m        ^^^^^^^^^\u{1b}[0m
",
        output
    );
}

#[test]
fn test_kanji_identifier() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test-kanji",
        r#"fn main(): void {
    let 今日 = "03.12.2017";
    今日 = "04.12.2017";
    今日 = 1;
}"#,
    );

    mini_rust::compile(&vec![source], &mut cursor).unwrap();
    let output = String::from_utf8(cursor.into_inner()).unwrap();

    assert_eq!(
        "error: Failed to lex token, because: Non ascii identifiers are currently not supported
--> test-kanji:2:9
  |
2 |    let 今日 = \"03.12.2017\";
  |\u{1b}[31m        ^^^\u{1b}[0m
error: Failed to lex token, because: Non ascii identifiers are currently not supported
--> test-kanji:3:5
  |
3 |    今日 = \"04.12.2017\";
  |\u{1b}[31m    ^^^\u{1b}[0m
error: Failed to lex token, because: Non ascii identifiers are currently not supported
--> test-kanji:4:5
  |
4 |    今日 = 1;
  |\u{1b}[31m    ^^^\u{1b}[0m
",
        output
    );
}

#[test]
fn test_valid_equality_operation_should_print_nothing() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test-equals",
        r#"fn main(): void {
 let s = "hello";
 let s2 = "world";
 let res = s == s2;
 }"#,
    );

    mini_rust::compile(&vec![source], &mut cursor).unwrap();
    let output = String::from_utf8(cursor.into_inner()).unwrap();

    assert_eq!("", output);
}

#[test]
fn test_invalid_equality_operation_should_return_error_message() {
    let mut cursor = Cursor::new(Vec::new());

    let source = Source::new(
        "test-equals",
        r#"fn main(): void {
 let s = "hello";
 let s2 = 2;
 let res = s == s2;
 }"#,
    );

    mini_rust::compile(&vec![source], &mut cursor).unwrap();
    let output = String::from_utf8(cursor.into_inner()).unwrap();

    let expected = "error: binary operation '==' cannot be applied to 'string' and 'i32'
--> test-equals:4:14
  |
4 | let res = s == s2;
  |\u{1b}[31m             ^^\u{1b}[0m - not allowed
";

    assert_eq!(expected, output);
}
