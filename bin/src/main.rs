use automata::{ts::ToDot, Pointed};
use automata_learning::passive::{
    sprout::{omega_sprout_conflicts, prefix_consistency_conflicts},
    OmegaSample,
};
use clap::{arg, command, Arg, ArgAction, Command};
use tracing::{debug, error, info, trace, Level};

fn conflicts_arg() -> Arg {
    Arg::new("conflicts")
        .short('c')
        .long("conflicts")
        .help("filename to output information on conflict relation")
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
        .subcommand(
            Command::new("passive")
                .subcommand_required(true)
                .short_flag('l')
                .arg(Arg::new("input").short('i').long("input"))
                .arg(Arg::new("output").short('o').long("output"))
                .arg(Arg::new("conflicts").short('c').long("output-conflicts"))
                .subcommand(
                    Command::new("rc")
                        .about("Learn a right congruence relation")
                        .arg(conflicts_arg()),
                )
                .subcommand(
                    Command::new("forc")
                        .about("Infer a family of right congruences (FORC) from the sample"),
                ),
        )
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
        Some(("passive", passive_matches)) => {
            let sample_lines: Vec<String> = match passive_matches.get_one::<String>("input") {
                None => {
                    debug!("No input files specified, using stdin");
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

            let output_dot = match OmegaSample::try_from(sample_lines) {
                Ok(sample) => match passive_matches.subcommand() {
                    Some(("rc", _)) => {
                        let conflicts = prefix_consistency_conflicts(sample);
                        let cong = omega_sprout_conflicts(conflicts, true);

                        if let Some(conflicts_target) =
                            passive_matches.get_one::<String>("conflicts")
                        {
                            debug!(
                                "Outputting conflict relation and congruences to {}",
                                conflicts_target
                            )
                        }

                        cong.dot_representation()
                    }
                    Some(("forc", _)) => {
                        todo!()
                    }
                    _ => unreachable!(),
                },
                Err(e) => {
                    panic!("Could not parse input sample: {}", e);
                }
            };

            match passive_matches.get_one::<String>("output") {
                None => {
                    debug!("No output file specified, using stdout");
                    println!("{}", output_dot);
                }
                Some(file_name) => {
                    debug!("Output file name specified: {:?}", file_name);
                    std::fs::write(file_name, output_dot).unwrap();
                }
            }
        }
        _ => unreachable!(),
    };
}
