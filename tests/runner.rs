use std::{fs, process::Command, str::from_utf8};

const NAME: &str = "test.s";

fn compile(name: &str) {
    Command::new("target/debug/kantan")
        .args(&[name, "--emit", "asm", "-o", NAME])
        .output()
        .expect("Failed to execute kantan");
}

fn link(name: &str) {
    let stderr = Command::new("gcc")
        .args(&[name, "-o", "test.exe"])
        .output()
        .expect("Failed to execute gcc")
        .stderr;

    if stderr.len() > 0 {
        let s = from_utf8(&stderr).unwrap().to_owned();
        eprintln!("{}", s);
    }

    assert_eq!(0, stderr.len());
}

fn execute() -> String {
    let stdout = Command::new("./test.exe")
        .output()
        .expect("Failed to execute test.exe")
        .stdout;

    from_utf8(&stdout).unwrap().to_owned()
}

fn valgrind() -> bool {
    Command::new("valgrind")
        .args(&[
            "--leak-check=full",
            "--error-exitcode=1",
            "--xml=yes",
            "--xml-file=valgrind.xml",
            "./test.exe",
        ])
        .status()
        .map(|exit| exit.success())
        .unwrap_or(false)
}

fn get_expected(name: &str) -> String {
    let name = format!("{}.expected", name.split(".").next().unwrap());
    fs::read_to_string(name).unwrap()
}

fn clean_up() {
    Command::new("rm")
        .args(&[NAME, "test.exe", "valgrind.xml"])
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
        link(NAME);
        let output = execute();
        let expected = get_expected(&name);
        let leak_free = valgrind();
        clean_up();
        assert_eq!(expected, output, "output != expected in {}", name);
        assert!(leak_free, "memory leaks in {}", name);
    }
}
