use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::process::Stdio;

#[derive(Serialize, Deserialize, Default, Debug, Clone, PartialEq)]
struct Expectation {
    compiler_stderr: String,
    compiler_exit_code: i32,
    program_stdout: String,
    program_stderr: String,
    program_exit_code: i32,
}

impl Expectation {
    fn normalize(&self) -> Self {
        let mut norm = self.clone();

        norm.compiler_stderr = norm.compiler_stderr.replace("\r\n", "\n");
        norm.program_stdout = norm.program_stdout.replace("\r\n", "\n");
        norm.program_stderr = norm.program_stderr.replace("\r\n", "\n");

        while norm.compiler_stderr.ends_with('\n') {
            norm.compiler_stderr.pop();
        }
        while norm.program_stdout.ends_with('\n') {
            norm.program_stdout.pop();
        }
        while norm.program_stderr.ends_with('\n') {
            norm.program_stderr.pop();
        }

        norm
    }
}

#[derive(Serialize, Deserialize, Default, Debug)]
struct AllExpectations {
    cases: HashMap<String, Expectation>,
}

fn build_f90c_exe() -> PathBuf {
    let exe = if cfg!(windows) {
        "target\\debug\\f90c.exe"
    } else {
        "target/debug/f90c"
    };
    if !Path::new(&exe).exists() {
        panic!("build first: cargo build");
    }
    PathBuf::from(exe)
}

fn compile_and_run(f90c: &Path, src: &Path) -> Expectation {
    use std::io::Write;

    let exe_path = if cfg!(windows) {
        PathBuf::from("tests/tmp_test.exe")
    } else {
        PathBuf::from("tests/tmp_test")
    };

    let mut exp = Expectation::default();
    let content = fs::read_to_string(src).expect("failed to read source file");

    let mut echo_input = String::new();
    let mut link_files = Vec::new();

    for line in content.lines() {
        let line = line.trim();
        if let Some(text) = line.strip_prefix("! echo: ") {
            echo_input.push_str(text.trim().trim_start_matches('"').trim_end_matches('"'));
            echo_input.push('\n');
        } else if let Some(link_file) = line.strip_prefix("! link-with:") {
            link_files.push(src.parent().unwrap().join(link_file.trim()));
        } else if !line.starts_with('!') {
            break;
        }
    }

    for link_path in &link_files {
        let link_out = Command::new(f90c)
            .arg(&link_path)
            .output()
            .expect("failed to compile link-with file");
        exp.compiler_stderr
            .push_str(&String::from_utf8_lossy(&link_out.stderr));
        if !link_out.status.success() {
            exp.compiler_exit_code = link_out.status.code().unwrap_or(-1);
            return exp;
        }
    }

    let mut comp_cmd = Command::new(f90c);
    if !link_files.is_empty() {
        for link_path in &link_files {
            let obj_name = link_path.with_extension(if cfg!(windows) { "obj" } else { "o" });
            comp_cmd.arg(obj_name);
        }
    }
    comp_cmd.arg(src).arg("-o").arg(&exe_path);

    let comp_out = comp_cmd.output().expect("failed to compile main file");
    exp.compiler_stderr
        .push_str(&String::from_utf8_lossy(&comp_out.stderr));
    exp.compiler_exit_code = comp_out.status.code().unwrap_or(-1);

    if exe_path.exists() && comp_out.status.success() {
        let mut run = Command::new(&exe_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("failed to run compiled executable");

        if !echo_input.is_empty() {
            if let Some(mut stdin) = run.stdin.take() {
                stdin
                    .write_all(echo_input.as_bytes())
                    .expect("failed to write to stdin");
            }
        }

        let output = run.wait_with_output().expect("failed to wait on program");
        exp.program_stdout
            .push_str(&String::from_utf8_lossy(&output.stdout));
        exp.program_stderr
            .push_str(&String::from_utf8_lossy(&output.stderr));
        exp.program_exit_code = output.status.code().unwrap_or(-1);
    }

    exp
}

fn expectations_path() -> PathBuf {
    PathBuf::from("tests/expectations.json")
}

fn load_all() -> AllExpectations {
    if let Ok(d) = fs::read_to_string(expectations_path()) {
        serde_json::from_str(&d).unwrap_or_default()
    } else {
        AllExpectations::default()
    }
}

fn save_all(all: &AllExpectations) {
    let mut items: Vec<_> = all.cases.iter().collect();
    items.sort_by_key(|(k, _)| k.get(0..2).and_then(|p| p.parse::<u8>().ok()).unwrap_or(0));

    let mut ordered = serde_json::Map::new();
    for (k, v) in items {
        ordered.insert(k.clone(), serde_json::to_value(v).unwrap());
    }

    let data = serde_json::to_string_pretty(&serde_json::json!({ "cases": ordered })).unwrap();
    fs::write(expectations_path(), data).unwrap();
}

fn list_test_files() -> Vec<PathBuf> {
    let mut files: Vec<_> = fs::read_dir("tests")
        .unwrap()
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| {
            p.extension()
                .map(|e| e.eq_ignore_ascii_case("f90"))
                .unwrap_or(false)
        })
        .filter(|p| {
            // include only files with stem like "01_name" .. "99_name"
            if let Some(stem_os) = p.file_stem() {
                let s = stem_os.to_string_lossy();
                if s.len() < 3 {
                    return false;
                }
                // third char must be '_'
                if s.as_bytes().get(2) != Some(&b'_') {
                    return false;
                }
                // first two chars must be digits and between 01 and 99
                let first2 = &s[0..2];
                if let Ok(n) = first2.parse::<u8>() {
                    return n >= 1 && n <= 99;
                }
            }
            false
        })
        .collect();
    files.sort();
    files
}

fn process_case(
    stem: &str,
    got: Expectation,
    all: &mut AllExpectations,
    record: bool,
    changed: &mut bool,
    failures: &mut Vec<String>,
) {
    if record || !all.cases.contains_key(stem) {
        all.cases.insert(stem.to_string(), got);
        println!("[recorded] {}", stem);
        *changed = true;
    } else {
        let exp = all.cases.get(stem).unwrap().normalize();
        let got = got.normalize();

        if got.compiler_exit_code != 0 {
            if exp.compiler_exit_code != got.compiler_exit_code {
                println!("---- {} ----", stem);
                println!(
                    "compiler exit expected {} got {}",
                    exp.compiler_exit_code, got.compiler_exit_code
                );
                if !got.compiler_stderr.is_empty() {
                    println!("compiler stderr:\n{}", got.compiler_stderr);
                }
                failures.push(stem.to_string());
                return;
            }
        }

        if exp != got {
            println!("---- {} ----", stem);
            if exp.compiler_exit_code != got.compiler_exit_code {
                println!(
                    "compiler exit expected {} got {}",
                    exp.compiler_exit_code, got.compiler_exit_code
                );
            }
            if exp.compiler_stderr != got.compiler_stderr {
                println!(
                    "compiler stderr diff\nEXPECTED:\n{}\nGOT:\n{}",
                    exp.compiler_stderr, got.compiler_stderr
                );
            }
            if exp.program_exit_code != got.program_exit_code {
                println!(
                    "program exit expected {} got {}",
                    exp.program_exit_code, got.program_exit_code
                );
            }
            if exp.program_stdout != got.program_stdout {
                println!(
                    "program stdout diff\nEXPECTED:\n{}\nGOT:\n{}",
                    exp.program_stdout, got.program_stdout
                );
            }
            if exp.program_stderr != got.program_stderr {
                println!(
                    "program stderr diff\nEXPECTED:\n{}\nGOT:\n{}",
                    exp.program_stderr, got.program_stderr
                );
            }
            failures.push(stem.to_string());
        } else {
            println!("[ok] {}", stem);
        }
    }
}

#[test]
fn compile_and_run_tests() {
    std::env::set_var("RUST_TEST_NOCAPTURE", "1");
    let mut record = false;
    for a in std::env::args().skip(1) {
        if a == "--record" {
            record = true;
        }
    }
    if std::env::var("TESTSUITE_RECORD") == Ok("1".into()) {
        record = true;
    }

    let f90c = build_f90c_exe();
    let mut all = load_all();
    let mut changed = false;
    let mut failures = Vec::new();

    for path in list_test_files() {
        let stem = path.file_stem().unwrap().to_string_lossy().to_string();
        let got = compile_and_run(&f90c, &path);
        process_case(&stem, got, &mut all, record, &mut changed, &mut failures);
    }

    if record && changed {
        save_all(&all);
    }
    if !record && failures.is_empty() && !expectations_path().exists() {
        save_all(&all);
    }

    if !failures.is_empty() {
        eprintln!("test failures: {:?}", failures);
        std::process::exit(1);
    }

    std::env::set_current_dir("tests").expect("set current dir to tests");
    let _ = Command::new("git").arg("clean").arg("-fdX").output();
}
