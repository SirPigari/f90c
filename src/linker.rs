use crate::cli::Cli;
use anyhow::Context;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[cfg(target_os = "windows")]
const LINKER_BYTES: &[u8] = include_bytes!("bin/tools/lld-link.exe");
#[cfg(not(target_os = "windows"))]
const LINKER_BYTES: &[u8] = include_bytes!("bin/tools/ld.lld");

#[cfg(all(windows, target_pointer_width = "64"))]
const MSVCRT_LIB: &[u8] = include_bytes!("libs/x64/msvcrt.lib");
#[cfg(all(windows, target_pointer_width = "64"))]
const KERNEL32_LIB: &[u8] = include_bytes!("libs/x64/kernel32.lib");
#[cfg(all(windows, target_pointer_width = "64"))]
const LEGACY_STDIO_LIB: &[u8] = include_bytes!("libs/x64/legacy_stdio_definitions.lib");

#[cfg(all(windows, target_pointer_width = "32"))]
const MSVCRT_LIB: &[u8] = include_bytes!("libs/x86/msvcrt.lib");
#[cfg(all(windows, target_pointer_width = "32"))]
const KERNEL32_LIB: &[u8] = include_bytes!("libs/x86/kernel32.lib");
#[cfg(all(windows, target_pointer_width = "32"))]
const LEGACY_STDIO_LIB: &[u8] = include_bytes!("libs/x86/legacy_stdio_definitions.lib");

#[cfg(all(not(windows), target_pointer_width = "64"))]
const LIBCXX_LIB: &[u8] = include_bytes!("libs/x64/libc++.so");
#[cfg(all(not(windows), target_pointer_width = "64"))]
const PATCH_LIB: &[u8] = include_bytes!("libs/x64/patch.o");

#[cfg(all(not(windows), target_pointer_width = "32"))]
const LIBCXX_LIB: &[u8] = include_bytes!("libs/x86/libc++.so");
#[cfg(all(not(windows), target_pointer_width = "32"))]
const PATCH_LIB: &[u8] = include_bytes!("libs/x86/patch.o");

#[cfg(not(target_os = "windows"))]
fn find_crt_files() -> anyhow::Result<(PathBuf, PathBuf, PathBuf)> {
    let candidates = [
        "/usr/lib/crt1.o",
        "/usr/lib64/crt1.o",
        "/usr/lib/x86_64-linux-gnu/crt1.o",
        "/usr/lib/gcc/x86_64-pc-linux-gnu/15.2.1/crtbegin.o",
    ];

    let crt1 = candidates.iter().find(|p| Path::new(p).exists());
    let crti = crt1.map(|p| Path::new(p).parent().unwrap().join("crti.o"));
    let crtn = crt1.map(|p| Path::new(p).parent().unwrap().join("crtn.o"));

    if let (Some(crt1), Some(crti), Some(crtn)) = (crt1, crti, crtn) {
        Ok((PathBuf::from(crt1), crti, crtn))
    } else {
        anyhow::bail!("Could not find CRT startup files. Install libc dev packages.");
    }
}

pub fn link(objects: &[PathBuf], out: &Path, cli: &Cli) -> anyhow::Result<()> {
    #[cfg(target_os = "windows")]
    let linker_path = {
        use std::process::Command;

        let cache_base = env::var_os("LOCALAPPDATA")
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from(r"C:\ProgramData"));
        let cache_dir = cache_base.join(".f90c-link");
        if !cache_dir.exists() {
            fs::create_dir_all(&cache_dir)?;
            let _ = Command::new("attrib")
                .args(&["+h", cache_dir.to_str().unwrap()])
                .status();
        }

        let arch = if cfg!(target_pointer_width = "64") {
            "x64"
        } else {
            "x86"
        };
        let arch_dir = cache_dir.join(arch);
        if !arch_dir.exists() {
            fs::create_dir_all(&arch_dir)?;
        }

        let linker_path = arch_dir.join("lld-link.exe");
        if !linker_path.exists() {
            println!("[link] writing linker to {}", linker_path.display());
            fs::write(&linker_path, LINKER_BYTES)?;
        }

        let libs = [
            ("msvcrt.lib", MSVCRT_LIB),
            ("kernel32.lib", KERNEL32_LIB),
            ("legacy_stdio_definitions.lib", LEGACY_STDIO_LIB),
        ];
        for (name, bytes) in libs {
            let path = arch_dir.join(name);
            if !path.exists() {
                fs::write(&path, bytes)?;
            }
        }

        linker_path
    };

    #[cfg(not(target_os = "windows"))]
    let linker_path = {
        use std::os::unix::fs::PermissionsExt;

        let home_dir = env::var_os("HOME")
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("/usr/local"));
        let cache_dir = home_dir.join(".f90c-link");
        if !cache_dir.exists() {
            fs::create_dir_all(&cache_dir)?;
        }

        let arch = if cfg!(target_pointer_width = "64") {
            "x64"
        } else {
            "x86"
        };
        let arch_dir = cache_dir.join(arch);
        if !arch_dir.exists() {
            fs::create_dir_all(&arch_dir)?;
        }

        let linker_path = arch_dir.join("ld.lld");
        if !linker_path.exists() {
            let mut f = fs::File::create(&linker_path)?;
            println!("[link] writing linker to {}", linker_path.display());
            use std::io::Write;
            f.write_all(LINKER_BYTES)?;
            fs::set_permissions(&linker_path, fs::Permissions::from_mode(0o755))?;
        }

        let libs = [
            ("libc++.so", LIBCXX_LIB),
            ("patch.o", PATCH_LIB),
        ];
        for (name, bytes) in libs {
            let path = arch_dir.join(name);
            if !path.exists() {
                fs::write(&path, bytes)?;
            }
        }

        linker_path
    };

    let mut args: Vec<String> = Vec::new();

    #[cfg(target_os = "windows")]
    {
        args.push(format!("/OUT:{}", out.display()));
        for obj in objects {
            if !cli.quiet {
                println!("[link] adding {}", obj.display());
            }
            args.push(obj.display().to_string());
        }
        if cli.lto {
            if !cli.quiet {
                println!("[link] enabling LTO");
            }
            args.push("/LTCG".into());
        }
        args.push("/SUBSYSTEM:CONSOLE".into());

        let lib_dir = linker_path.parent().unwrap();
        args.push(format!("/LIBPATH:{}", lib_dir.display()));
        args.push("msvcrt.lib".into());
        args.push("legacy_stdio_definitions.lib".into());
        args.push("kernel32.lib".into());
    }

    #[cfg(unix)]
    {
        args.push("--gc-sections".into());
        args.push("--strip-all".into());
        args.push("--sort-section=alignment".into());

        if cli.lto {
            if !cli.quiet {
                println!("[link] enabling LTO");
            }
            args.push("-flto".into());
        }

        args.push("-no-pie".into());

        let (crt1, crti, crtn) = find_crt_files()?;
        if !cli.quiet {
            println!(
                "[link] using crt files: {}, {}, {}",
                crt1.display(),
                crti.display(),
                crtn.display()
            );
        }

        args.push("-o".into());
        args.push(out.display().to_string());

        args.push(crt1.display().to_string());
        args.push(crti.display().to_string());

        for obj in objects {
            if !cli.quiet {
                println!("[link] adding {}", obj.display());
            }
            args.push(obj.display().to_string());
        }

        args.push(crtn.display().to_string());

        let lib_dir = linker_path.parent().unwrap();

        args.push(lib_dir.join("patch.o").display().to_string());
        args.push("--dynamic-linker".into());
        args.push("/lib64/ld-linux-x86-64.so.2".into());

        args.push(format!("-L{}", lib_dir.display()));
        args.push("-lc++".into());
        args.push("-lc".into());
        args.push("-lm".into());
    }

    if !cli.quiet {
        println!("[link] invoking linker {}", env!("LINKER_VERSION"));
    }
    let status = Command::new(&linker_path)
        .args(&args)
        .status()
        .with_context(|| format!("failed to execute linker at {:?}", linker_path))?;

    if !status.success() {
        anyhow::bail!(
            "linker exited with status {:?}",
            status.code().unwrap_or(-1)
        );
    }

    if !cli.quiet {
        println!("[link] successfully linked {}", out.display());
    }

    Ok(())
}
