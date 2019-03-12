use std::{fs, process::Command, str::from_utf8};

fn compile(name: &str) {
    Command::new("target/debug/kantan")
        .args(&[name])
        .output()
        .expect("Failed to execute kantan");
}

fn link(name: &str) {
    let stderr = Command::new("gcc")
        .args(&[name, "-o", "test.exe"])
        .output()
        .expect("Failed to execute gcc")
        .stderr;

    assert_eq!(0, stderr.len());
}

fn execute() -> String {
    let stdout = Command::new("./test.exe")
        .output()
        .expect("Failed to execute test.exe")
        .stdout;

    from_utf8(&stdout).unwrap().to_owned()
}

fn get_expected(name: &str) -> String {
    let name = format!("{}.expected", name.split(".").next().unwrap());
    fs::read_to_string(name).unwrap()
}

fn clean_up() {
    Command::new("rm")
        .args(&["test.s", "test.exe"])
        .output()
        .expect("Failed to execute rm");
}

#[test]
fn test_all_files() {
    let files = fs::read_dir("tests/files").unwrap();

    for file in files {
        let name = file.unwrap().path().display().to_string();

        if name.ends_with(".expected") {
            continue;
        }

        compile(&name);
        link("test.s");
        let output = execute();
        let expected = get_expected(&name);
        clean_up();
        assert_eq!(expected, output);
    }
}
