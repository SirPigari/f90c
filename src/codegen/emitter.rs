use anyhow::Result;
use std::path::Path;

pub fn write_object_file(bytes: &[u8], out: &Path) -> Result<()> {
    std::fs::write(out, bytes)?;
    Ok(())
}
