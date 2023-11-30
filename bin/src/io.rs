use std::io::Read;

use automata::{automata::DeterministicOmegaAutomaton, hoa::HoaAlphabet, TransitionSystem};
use clap::ArgMatches;

pub fn to_file_or_stdout(maybe_file_name: Option<&String>, output: &str) -> anyhow::Result<()> {
    if let Some(file_name) = maybe_file_name {
        std::fs::write(file_name, output)?;
        Ok(())
    } else {
        println!("{output}");
        Ok(())
    }
}

pub fn hoa_deterministic(matches: &ArgMatches) -> Vec<DeterministicOmegaAutomaton<HoaAlphabet>> {
    let hoa = from_file_or_stdin(matches.get_one("input"));
    let parsed_auts = automata::hoa::hoa_to_ts(&hoa);
    let mut auts = vec![];
    for nondet_aut in &parsed_auts {
        if let Some(aut) = nondet_aut.to_deterministic() {
            tracing::debug!("Parsed HOA automaton with {} states", aut.size());
            auts.push(aut);
        } else {
            tracing::error!("Input is not deterministic!");
        }
    }
    tracing::info!("Parsed {} HOA automata", auts.len());
    auts
}

pub fn from_file_or_stdin(maybe_file_name: Option<&String>) -> String {
    match maybe_file_name {
        Some(f) => std::fs::read_to_string(f).expect("Unable to read file"),
        None => {
            tracing::debug!("No input files specified, using stdin");
            let mut buf = String::new();
            std::io::stdin()
                .read_to_string(&mut buf)
                .expect("Error when reading from stdin");
            buf
        }
    }
}
