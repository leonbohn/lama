use std::io::Read;

use automata::parse_hoa;
use clap::{command, Arg, Command};
use tracing::{info, trace};
use tracing_subscriber::fmt;

fn main() {
    // Set up the tracing subscriber
    let subscriber = fmt::Subscriber::builder().finish();

    tracing::subscriber::set_global_default(subscriber)
        .expect("Unable to set global tracing subscriber");

    let matches = command!()
        .propagate_version(true)
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(Command::new("parse").about("Parses input"))
        .arg(
            Arg::new("infile")
                // .about("Can be set to read from input file instead of stdin")
                .short('i')
                .long("input-file")
                .value_name("infile"),
        )
        .get_matches();

    let mut input = String::new();
    if let Some(filename) = matches.get_one::<String>("infile") {
        info!("Reading input from file {}", filename);
        let mut file = std::fs::File::open(filename).expect("Could not open file");

        file.read_to_string(&mut input).unwrap();
    } else {
        info!("Reading input from stdin");
        std::io::stdin().read_to_string(&mut input).unwrap();
    }
    println!("{}", input);

    match matches.subcommand() {
        Some(("parse", _sub_matches)) => {
            trace!("Parsing input");
            let aut = parse_hoa(&input).expect("Could not parse input");
            println!("{}", aut);
        }
        _ => unreachable!("clap should have prevented this"),
    }
}
