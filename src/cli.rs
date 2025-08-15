use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt, Clone)]
#[structopt(
    name = "f90c",
    about = "Single executable Fortran 90 compiler written in Rust"
)]
pub struct Cli {
    /// Positional inputs (sources .f90 and/or existing object files .o/.obj) when no subcommand is used
    /// Example: f90c main.f90 lib1.obj other.o
    /// All .f90 inputs are compiled then linked together with any object inputs.
    pub inputs: Vec<PathBuf>,
    /// Output executable path (when no subcommand) or object path with Build command
    #[structopt(
        short = "o",
        long,
        help = "Output object/executable path (defaults derived from first source)"
    )]
    pub out: Option<PathBuf>,
    /// Enable all warnings
    #[structopt(long = "Wall", help = "Enable all warnings")]
    pub wall: bool,
    /// Treat warnings as errors
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

    #[structopt(long = "help", short = "h", help = "Show this help message")]
    pub help: bool,

    #[structopt(subcommand)]
    pub cmd: Option<Command>,

    #[structopt(long = "version", short = "v", help = "Show version information")]
    pub version: bool,
}

#[derive(Debug, StructOpt, Clone)]
pub enum Command {
    /// Lex only: dump tokens
    Lex {
        /// Input .f90 file
        input: PathBuf,
    },
    /// Parse only: dump AST
    Parse {
        /// Input .f90 file
        input: PathBuf,
    },
    /// Run semantic checks only
    Check {
        /// Input .f90 file
        input: PathBuf,
    },
    /// Build object file (.o/.obj)
    Build {
        /// Input .f90 file
        input: PathBuf,
        /// Output object path (defaults next to input with .obj/.o)
        #[structopt(short = "o", long)]
        out: Option<PathBuf>,

        #[structopt(long)]
        run: bool,
    },
    /// Link object(s) into an executable
    Link {
        /// Input objects or sources; if source, it will be compiled first to an object alongside
        inputs: Vec<PathBuf>,
        /// Output executable path
        #[structopt(short = "o", long)]
        out: Option<PathBuf>,
    },
    /// Emit object file to a specific path
    EmitObj {
        /// Input .f90 file
        input: PathBuf,
        /// Output object path
        out: PathBuf,
    },
    /// Parse, check, and prepare to run (JIT TBD)
    Run {
        /// Input .f90 file
        input: PathBuf,
    },
    Help,
}
