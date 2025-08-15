use anyhow::Result;

use crate::ir::Module;

pub trait Backend {
    fn compile(&self, module: &Module) -> Result<Vec<u8>>; // returns object bytes
}

pub mod cranelift;
pub mod emitter;
