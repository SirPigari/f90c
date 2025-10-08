use anyhow::Context;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

#[cfg(target_os = "windows")]
const LINKER_BYTES: &[u8] = include_bytes!("bin/lld-link.exe");
#[cfg(not(target_os = "windows"))]
const LINKER_BYTES: &[u8] = include_bytes!("bin/ld.lld");

#[cfg(target_pointer_width = "64")]
const MSVCRT_LIB: &[u8] = include_bytes!("libs/x64/msvcrt.lib");
#[cfg(target_pointer_width = "64")]
const KERNEL32_LIB: &[u8] = include_bytes!("libs/x64/kernel32.lib");
#[cfg(target_pointer_width = "64")]
const LEGACY_STDIO_LIB: &[u8] = include_bytes!("libs/x64/legacy_stdio_definitions.lib");

#[cfg(target_pointer_width = "32")]
const MSVCRT_LIB: &[u8] = include_bytes!("libs/x86/msvcrt.lib");
#[cfg(target_pointer_width = "32")]
const KERNEL32_LIB: &[u8] = include_bytes!("libs/x86/kernel32.lib");
#[cfg(target_pointer_width = "32")]
const LEGACY_STDIO_LIB: &[u8] = include_bytes!("libs/x86/legacy_stdio_definitions.lib");

pub fn link(objects: &[PathBuf], out: &Path, lto: bool) -> anyhow::Result<()> {
    let temp_root = std::env::temp_dir();
    let uid = uuid::Uuid::new_v4();

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

    let mut args: Vec<String> = if cfg!(windows) {
        let mut v = vec![format!("/OUT:{}", out.display())];

        for obj in objects {
            v.push(obj.display().to_string());
        }

        if lto {
            v.push("/LTCG".into());
        }

        v.push("/SUBSYSTEM:CONSOLE".into());

        v
    } else {
        let mut v = vec!["-o".into(), out.display().to_string()];

        if lto {
            v.push("-flto".into());
        }

        for obj in objects {
            v.push(obj.display().to_string());
        }

        v
    };

    #[cfg(target_os = "windows")]
    {
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

    println!("[link] successfully linked {}", out.display());

    let _ = fs::remove_file(&linker_path);
    #[cfg(target_os = "windows")]
    {
        let _ = fs::remove_dir_all(temp_root.join(format!("lucia_libs_{}", uid)));
    }

    Ok(())
}
