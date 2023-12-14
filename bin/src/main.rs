use automata::{
    congruence::FORC,
    ts::{dot::display_dot, Dottable},
    Map, TransitionSystem,
};
use automata_learning::passive::{
    sprout::{iteration_consistency_conflicts, prefix_consistency_conflicts, sprout},
    OmegaSample,
};
use clap::{command, Arg, ArgAction, Command};
use io::{hoa_deterministic, to_file_or_stdout};
use tracing::{debug, error, Level};

use crate::io::from_file_or_stdin;

mod io;

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
                .arg(Arg::new("show").short('s').long("show").action(ArgAction::SetTrue))
                .arg(Arg::new("input").short('i').long("input"))
                .arg(Arg::new("output").short('o').long("output"))
                .arg(Arg::new("nooutput").long("no-output").action(ArgAction::SetTrue).help("Skip producing output and simply forget it"))
                .arg(Arg::new("conflicts").help("Prefix for outputting the conflict relation(s). If a single conflict relation is used (e.g. when learning a right congruence), this is a single image. If multiple conflict relations are used (e.g. when learning a FORC), then we append the name of the respective conflict relation to the prefix.").short('c').long("output-conflicts"))
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
        .subcommand(Command::new("tosample").arg(Arg::new("input").short('i').long("input")))
        .subcommand(
            Command::new("nop")
                .arg(Arg::new("input").short('i').long("input"))
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
        Some(("tosample", sample_matches)) => {
            let auts = hoa_deterministic(sample_matches);
            if auts.len() > 1 {
                tracing::error!("This operation supports only single inputs");
                std::process::exit(-1);
            }
            std::process::exit(0)
        }
        Some(("nop", nop_matches)) => {
            let hoa = from_file_or_stdin(nop_matches.get_one("input"));
            let parsed_auts = automata::hoa::input::hoa_to_ts(&hoa);
            let mut auts = vec![];
            for nondet_aut in &parsed_auts {
                if let Some(aut) = nondet_aut.to_deterministic() {
                    tracing::debug!("Parsed HOA automaton with {} states", aut.size());
                    auts.push(aut);
                } else {
                    tracing::error!("Input is not deterministic!");
                }
            }
            println!("Successfully parsed {} HOA automata", auts.len());
        }

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

                        if let Some(conflicts_target) =
                            passive_matches.get_one::<String>("conflicts")
                        {
                            debug!("Outputting conflict relation to {}", conflicts_target);
                            // conflicts
                            //     .render_to_file_name(conflicts_target)
                            //     .expect("Unable to render conflict relation to file");
                            todo!()
                        }

                        let cong = sprout(conflicts, vec![], true);

                        if passive_matches.get_flag("nooutput") {
                            "".to_string()
                        } else {
                            // cong.dot_representation()
                            todo!()
                        }
                    }
                    Some(("forc", _)) => {
                        let cong = sample.infer_right_congruence();
                        let split_sample = sample.split(&cong);

                        let conflict_relations: Map<_, _> = split_sample
                            .classes()
                            .map(|c| {
                                (
                                    c.clone(),
                                    iteration_consistency_conflicts(&split_sample, c.clone()),
                                )
                            })
                            .collect();

                        if let Some(conflicts_target) =
                            passive_matches.get_one::<String>("conflicts")
                        {
                            debug!("Outputting conflict relations to {}", conflicts_target);
                            for (class, conflicts) in &conflict_relations {
                                let file_name =
                                    format!("{}-{}.dot", conflicts_target, class.mr_to_string());
                                // conflicts
                                //     .render_to_file_name(&file_name)
                                //     .expect("Unable to render conflict relation to file");
                                todo!()
                            }
                        }
                        let progress: Vec<_> = conflict_relations
                            .into_iter()
                            .map(|(c, conflicts)| {
                                (
                                    cong.get(c).unwrap(),
                                    sprout(
                                        conflicts,
                                        vec![],
                                        // SeparatesIdempotents::new(
                                        //     split_sample.get(&c).expect("This must exist"),
                                        // ),
                                        false,
                                    ),
                                )
                            })
                            .collect();
                        let forc = FORC::from_iter(cong, progress);

                        if passive_matches.get_flag("nooutput") {
                            "".to_string()
                        } else {
                            // forc.dot_representation()
                            todo!()
                        }
                    }
                    _ => unreachable!(),
                },
                Err(e) => {
                    panic!("Could not parse input sample: {}", e);
                }
            };

            if passive_matches.get_flag("show") {
                match display_dot(&output_dot) {
                    Ok(_) => {}
                    Err(e) => {
                        error!("Could not display dot: {}", e);
                    }
                };
            }
            to_file_or_stdout(passive_matches.get_one("output"), &output_dot)
                .expect("Could not write output!");
        }
        _ => unreachable!(),
    };
}
