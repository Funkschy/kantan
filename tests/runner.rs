use std::{ffi::OsStr, fs, path::PathBuf, process::Command, str::from_utf8};

const NAME: &str = "test.s";

fn compile_dir(name: &str) {
    let paths = fs::read_dir(name)
        .unwrap()
        .into_iter()
        .map(|f| f.unwrap().path())
        .collect::<Vec<PathBuf>>();

    let files = paths
        .iter()
        .map(|p| p.file_name().unwrap())
        .collect::<Vec<&OsStr>>();

    let mut files = files
        .iter()
        .map(|f| f.to_str().unwrap())
        .collect::<Vec<&str>>();

    let out = format!("../../../{}", NAME);
    let mut args = vec!["--emit", "asm", "-o", &out];
    files.append(&mut args);

    Command::new("../../../target/debug/kantan")
        .current_dir(name)
        .args(&files)
        .output()
        .expect("Failed to execute kantan");
}

fn compile(name: &str) {
    Command::new("target/debug/kantan")
        // small optimization level
        .args(&[name, "--emit", "asm", "-o", NAME, "-O", "1"])
        .output()
        .expect("Failed to execute kantan");
}

fn link(name: &str, program_name: &str) {
    let stderr = Command::new("gcc")
        .args(&[name, "-o", "test.exe"])
        .output()
        .expect("Failed to execute gcc")
        .stderr;

    if stderr.len() > 0 {
        let s = from_utf8(&stderr).unwrap().to_owned();
        eprintln!("{}", s);
    }

    assert_eq!(0, stderr.len(), "Linking failed for {}", program_name);
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
        let path = file.unwrap().path();
        let name = path.display().to_string();

        if name.ends_with(".expected") {
            continue;
        }

        if path.is_dir() {
            compile_dir(&name);
        } else {
            compile(&name);
        }
        link(NAME, &name);
        let output = execute();
        let expected = get_expected(&name);
        let leak_free = valgrind();
        clean_up();
        assert_eq!(expected, output, "output != expected in {}", name);
        assert!(leak_free, "memory leaks in {}", name);
    }
}
