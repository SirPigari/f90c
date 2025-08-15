use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let bin_dir = manifest_dir.join("src").join("bin");

    let mut bundle_files: Vec<&str> = vec![];

    if cfg!(windows) {
        bundle_files.extend(["lld-link.exe"]);
    } else {
        bundle_files.extend(["ld.lld", "libc++.so", "libLLVM-20.so"]);
    }

    for file_name in bundle_files {
        let src_path = bin_dir.join(file_name);
        let dest_path = out_dir.join(file_name);

        fs::copy(&src_path, &dest_path).unwrap_or_else(|_| {
            panic!(
                "Failed to copy {} to {}",
                src_path.display(),
                dest_path.display()
            )
        });

        println!("cargo:rerun-if-changed={}", src_path.display());
    }

    println!("cargo:rustlog=info,cranelift_object=off,cranelift_codegen=off");
    println!("cargo:rerun-if-changed=src/fortran.lalrpop");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/bin");

    lalrpop::process_root().unwrap();

    let linker_version = if cfg!(windows) {
        Command::new(bin_dir.join("lld-link.exe"))
            .arg("--version")
            .output()
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
            .unwrap_or_else(|_| "unknown".to_string())
    } else {
        Command::new(bin_dir.join("ld.lld"))
            .arg("--version")
            .output()
            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
            .unwrap_or_else(|_| "unknown".to_string())
    };

    println!("cargo:rustc-env=LINKER_VERSION={}", linker_version);
}
