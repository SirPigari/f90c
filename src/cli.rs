use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt, Clone)]
#[structopt(
    name = "f90c",
    about = "Single executable Fortran 90 compiler written in Rust"
)]
pub struct Cli {
    pub inputs: Vec<PathBuf>,

    #[structopt(
        short = "o",
        long,
        help = "Output object/executable path (defaults derived from first source)"
    )]
    pub out: Option<PathBuf>,

    #[structopt(long = "Wall", help = "Enable all warnings")]
    pub wall: bool,

    #[structopt(long = "Werror", help = "Treat warnings as errors")]
    pub werror: bool,

    #[structopt(
        long = "lto",
        help = "Enable Link Time Optimization (LTO) for better performance"
    )]
    pub lto: bool,

    #[structopt(
        short = "O",
        long = "opt-level",
        value_name = "LEVEL",
        default_value = "2",
        possible_values = &["0","1","2","3","s","z","S","Z"],
        help = "\
    Optimization level:
    0 = none
    1 = speed (quick)
    2 = speed (default)
    3 = speed (verifier off)
    s = speed and size
    z = smallest size
    S = speed and size (aggressive)
    Z = smallest size (aggressive)"
    )]
    pub opt_level: String,

    #[structopt(
        long = "quiet",
        short = "q",
        help = "Suppress all output except errors"
    )]
    pub quiet: bool,

    #[structopt(long = "debug-ir", help = "Show IR before and after optimizations")]
    pub debug_ir: bool,

    #[structopt(short = "S", long = "emit-asm", help = "Print generated assembly")]
    pub emit_asm: bool,

    #[structopt(long = "help", short = "h", help = "Show this help message")]
    pub help: bool,

    #[structopt(subcommand)]
    pub cmd: Option<Command>,

    #[structopt(long = "version", short = "v", help = "Show version information")]
    pub version: bool,

    #[structopt(
        long = "run",
        help = "Run after building (only applies if no subcommand)"
    )]
    pub run: bool,
}

#[derive(Debug, StructOpt, Clone)]
pub enum Command {
    Lex {
        input: PathBuf,
        out: Option<PathBuf>,
    },
    Parse {
        input: PathBuf,
        out: Option<PathBuf>,
    },
    Check {
        input: PathBuf,
    },
    Build {
        input: PathBuf,
        #[structopt(short = "o", long)]
        out: Option<PathBuf>,
        #[structopt(long)]
        run: bool,
    },
    Link {
        inputs: Vec<PathBuf>,
        #[structopt(short = "o", long)]
        out: Option<PathBuf>,
    },
    EmitObj {
        input: PathBuf,
        out: PathBuf,
    },
    Run {
        input: PathBuf,
    },
    Help,
}

impl Cli {
    pub fn normalize(mut self) -> Self {
        if self.cmd.is_none() {
            if self.inputs.len() > 1 {
                self.cmd = Some(Command::Link {
                    inputs: self.inputs.clone(),
                    out: self.out.clone(),
                });
            } else if let Some(first) = self.inputs.get(0) {
                self.cmd = Some(Command::Build {
                    input: first.clone(),
                    out: self.out.clone(),
                    run: self.run,
                });
            }
        }
        if let Some(Command::Lex {
            ref input,
            out: ref output,
        }) = self.cmd
        {
            if output.is_none() {
                let out = self.out.clone();
                self.cmd = Some(Command::Lex {
                    input: input.to_path_buf(),
                    out,
                });
            }
        }
        if let Some(Command::Parse {
            ref input,
            out: ref output,
        }) = self.cmd
        {
            if output.is_none() {
                let out = self.out.clone();
                self.cmd = Some(Command::Parse {
                    input: input.to_path_buf(),
                    out,
                });
            }
        }
        self
    }
}
