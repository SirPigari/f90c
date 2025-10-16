use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let bin_dir = manifest_dir.join("src").join("bin").join("tools");
    let lib_dir =
        manifest_dir
            .join("src")
            .join("libs")
            .join(if cfg!(target_pointer_width = "64") {
                "x64"
            } else {
                "x86"
            });

    let mut bundle_files_bin: Vec<&str> = vec![];
    let mut bundle_files_lib: Vec<&str> = vec![];

    if cfg!(windows) {
        bundle_files_bin.extend(["lld-link.exe", "objdump.exe"]);
        bundle_files_lib.extend(["kernel32.lib", "legacy_stdio_definitions.lib", "msvcrt.lib"]);
    } else {
        bundle_files_bin.extend(["ld.lld"]);
        bundle_files_lib.extend(["libc++.so", "patch.o"]);
    }

    for file_name in bundle_files_bin {
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
    for file_name in bundle_files_lib {
        let src_path = lib_dir.join(file_name);
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
