use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
pub struct Lama {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    #[command(about = "Parses HOA input and prints the parsed thing as HOA")]
    Parse { input_file: Option<String> },
    #[command(about = "Runs the web interface on the given port")]
    Web { port: Option<u16> },
}
