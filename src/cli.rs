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

    #[structopt(long = "help", short = "h", help = "Show this help message")]
    pub help: bool,

    #[structopt(subcommand)]
    pub cmd: Option<Command>,

    #[structopt(long = "version", short = "v", help = "Show version information")]
    pub version: bool,
}

#[derive(Debug, StructOpt, Clone)]
pub enum Command {
    Lex {
        input: PathBuf,
    },

    Parse {
        input: PathBuf,
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
