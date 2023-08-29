use automata_learning::passive::OmegaSample;
use clap::{arg, command, Arg, ArgAction, Command};
use tracing::{debug, error, info, trace, Level};

fn infer_command() -> Command {
    Command::new("rc")
        .about("Learn a right congruence relation")
        .arg(Arg::new("input").short('i').long("input"))
        .arg(Arg::new("output").short('o').long("output"))
}

fn main() {
    let matches = command!()
        .arg(
            Arg::new("verbose")
                .short('v')
                .long("verbose")
                .help("Enable verbose logging")
                .action(ArgAction::SetTrue)
                .conflicts_with("debug"),
        )
        .arg(
            Arg::new("debug")
                .short('d')
                .long("debug")
                .help("Turn on debugging information")
                .action(ArgAction::SetTrue)
                .conflicts_with("verbose"),
        )
        .subcommand(infer_command())
        .subcommand_required(true)
        .get_matches();

    let level = if matches.get_flag("verbose") {
        Level::TRACE
    } else if matches.get_flag("debug") {
        Level::DEBUG
    } else {
        Level::INFO
    };
    let subscriber = tracing_subscriber::fmt()
        .compact()
        .with_level(true)
        .with_max_level(level)
        .finish();
    tracing::subscriber::set_global_default(subscriber).unwrap();

    match matches.subcommand() {
        Some(("rc", infer_matches)) => {
            let raw_sample: Vec<String> = match infer_matches.get_one::<String>("input") {
                None => {
                    info!("No input files specified, using stdin");
                    std::io::stdin().lines().map(|line| line.unwrap()).collect()
                }
                Some(file_name) => {
                    debug!("Input file name specified: {:?}", file_name);
                    std::fs::read_to_string(file_name)
                        .unwrap()
                        .lines()
                        .map(|s| s.to_string())
                        .collect()
                }
            };
            let sample = OmegaSample::try_from(raw_sample);
            println!("{:?}", sample);
        }
        _ => unreachable!(),
    };

    info!("Hello, world!");
    debug!("Hello, world!");
    trace!("Hello, world!");
    error!("Hello, world!");
}
