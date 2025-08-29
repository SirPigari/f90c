use anyhow::Context;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

#[cfg(target_os = "windows")]
const LINKER_BYTES: &[u8] = include_bytes!("bin/lld-link.exe");
#[cfg(not(target_os = "windows"))]
const LINKER_BYTES: &[u8] = include_bytes!("bin/ld.lld");

pub fn link(objects: &[PathBuf], out: &Path, lto: bool) -> anyhow::Result<()> {
    let temp_dir = std::env::temp_dir();

    let linker_path = {
        let mut path = temp_dir.clone();
        let filename = if cfg!(windows) {
            format!("lld-link-{}.exe", uuid::Uuid::new_v4())
        } else {
            format!("lld-{}", uuid::Uuid::new_v4())
        };
        path.push(filename);
        path
    };

    let mut linker_file = fs::File::create(&linker_path)
        .with_context(|| format!("Failed to create linker at {:?}", linker_path))?;
    linker_file.write_all(LINKER_BYTES)?;
    drop(linker_file);

    #[cfg(unix)]
    fs::set_permissions(&linker_path, fs::Permissions::from_mode(0o755))?;
    let args: Vec<String> = if cfg!(windows) {
        let mut v = vec![format!("/OUT:{}", out.display())];

        for obj in objects {
            println!("[link] reading object file {}", obj.display());
            v.push(obj.to_string_lossy().to_string());
        }

        if lto {
            println!("[link] enabling LTO with /LTCG");
            v.push("/LTCG".to_string());
        }

        println!("[link] linking with dynamic CRT (msvcrt.lib)");
        v.push("msvcrt.lib".to_string());
        v.push("legacy_stdio_definitions.lib".to_string());
        v.push("kernel32.lib".to_string());

        v
    } else {
        let mut v = vec!["-o".to_string(), out.to_string_lossy().to_string()];

        if lto {
            println!("[link] enabling LTO with -flto");
            v.push("-flto".to_string());
        }

        for obj in objects {
            println!("[link] reading object file {:?}", obj);
            v.push(obj.to_string_lossy().to_string());
        }
        v
    };

    let status = Command::new(&linker_path)
        .args(&args)
        .status()
        .with_context(|| format!("Failed to execute linker at {:?}", linker_path))?;

    if !status.success() {
        anyhow::bail!(
            "[link] linker exited with status {:?}",
            status.code().unwrap_or(0)
        );
    }

    println!("[link] successfully linked {}", out.display());
    Ok(())
}
