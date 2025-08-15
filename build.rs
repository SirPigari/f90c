use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());

    let bin_dir = manifest_dir.join("src").join("bin");

    // Files to always copy
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
}
