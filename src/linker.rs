use anyhow::Context;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

#[cfg(target_os = "windows")]
const LINKER_BYTES: &[u8] = include_bytes!("bin/lld-link.exe");
#[cfg(not(target_os = "windows"))]
const LINKER_BYTES: &[u8] = include_bytes!("bin/ld.lld");

// Windows libs
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

// Unix libs
#[cfg(all(not(windows), target_pointer_width = "64"))]
const LIBCXX_LIB: &[u8] = include_bytes!("libs/x64/libc++.so");
#[cfg(all(not(windows), target_pointer_width = "64"))]
const LLVM_LIB: &[u8] = include_bytes!("libs/x64/libLLVM-20.so");
#[cfg(all(not(windows), target_pointer_width = "64"))]
const PATCH_LIB: &[u8] = include_bytes!("libs/x64/patch.o");

#[cfg(all(not(windows), target_pointer_width = "32"))]
const LIBCXX_LIB: &[u8] = include_bytes!("libs/x86/libc++.so");
#[cfg(all(not(windows), target_pointer_width = "32"))]
const LLVM_LIB: &[u8] = include_bytes!("libs/x86/libLLVM-20.so");
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

pub fn link(objects: &[PathBuf], out: &Path, lto: bool) -> anyhow::Result<()> {
    let temp_root = std::env::temp_dir();
    let uid = uuid::Uuid::new_v4();

    // Write linker to temp
    let linker_path = {
        let mut path = temp_root.clone();
        let filename = if cfg!(windows) {
            format!("lld-link-{}.exe", uid)
        } else {
            format!("lld-{}", uid)
        };
        path.push(filename);
        path
    };
    {
        let mut f = fs::File::create(&linker_path)
            .with_context(|| format!("failed to create linker at {:?}", linker_path))?;
        f.write_all(LINKER_BYTES)?;
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(&linker_path, fs::Permissions::from_mode(0o755))?;
    }

    let mut args: Vec<String> = Vec::new();

    #[cfg(target_os = "windows")]
    {
        args.push(format!("/OUT:{}", out.display()));
        for obj in objects {
            println!("[link] adding {}", obj.display());
            args.push(obj.display().to_string());
        }
        if lto {
            println!("[link] enabling LTO");
            args.push("/LTCG".into());
        }
        args.push("/SUBSYSTEM:CONSOLE".into());

        let lib_dir = temp_root.join(format!("lucia_libs_{}", uid));
        fs::create_dir_all(&lib_dir)?;
        fs::write(lib_dir.join("msvcrt.lib"), MSVCRT_LIB)?;
        fs::write(lib_dir.join("legacy_stdio_definitions.lib"), LEGACY_STDIO_LIB)?;
        fs::write(lib_dir.join("kernel32.lib"), KERNEL32_LIB)?;

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
        //args.push("--no-warnings".into());
        if lto {
            println!("[link] enabling LTO");
            args.push("-flto".into());
        }

        args.push("-no-pie".into());
        let (crt1, crti, crtn) = find_crt_files()?;
        println!("[link] using crt files: {}, {}, {}", crt1.display(), crti.display(), crtn.display());

        args.push("-o".into());
        args.push(out.display().to_string());

        args.push(crt1.display().to_string());
        args.push(crti.display().to_string());

        for obj in objects {
            println!("[link] adding {}", obj.display());
            args.push(obj.display().to_string());
        }

        args.push(crtn.display().to_string());

        let lib_dir = temp_root.join(format!("lucia_libs_{}", uid));
        fs::create_dir_all(&lib_dir)?;
        fs::write(lib_dir.join("libc++.so"), LIBCXX_LIB)?;
        fs::write(lib_dir.join("libLLVM-20.so"), LLVM_LIB)?;
        
        let patch_path = lib_dir.join("patch.o");
        fs::write(&patch_path, PATCH_LIB)?;

        args.push(patch_path.display().to_string());

        args.push("--dynamic-linker".into());
        args.push("/lib64/ld-linux-x86-64.so.2".into());

        args.push(format!("-L{}", lib_dir.display()));
        args.push("-lc++".into());
        args.push("-lLLVM-20".into());
        args.push("-lc".into());
        args.push("-lm".into());
    }

    println!("[link] invoking linker {}", env!("LINKER_VERSION"));
    let status = Command::new(&linker_path)
        .args(&args)
        .status()
        .with_context(|| format!("failed to execute linker at {:?}", linker_path))?;

    if !status.success() {
        anyhow::bail!("linker exited with status {:?}", status.code().unwrap_or(-1));
    }

    println!("[link] successfully linked {}", out.display());

    let _ = fs::remove_file(&linker_path);
    #[cfg(target_os = "windows")]
    {
        let _ = fs::remove_dir_all(temp_root.join(format!("lucia_libs_{}", uid)));
    }

    Ok(())
}
