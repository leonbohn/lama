use std::fmt::Display;

use hoars::{
    AcceptanceAtom, AcceptanceCondition, AcceptanceInfo, FromHoaError, HoaAutomaton, HoaSymbol, Id,
};
use tracing::{debug, info, trace};

use crate::{
    BuchiCondition, Combined, Dba, Deterministic, Dpa, Growable, OmegaAutomaton, OmegaCondition,
    ParityCondition, Set, Symbol,
};

impl Symbol for HoaSymbol {}

impl TryFrom<HoaAutomaton> for ParityCondition<(Id, HoaSymbol)> {
    type Error = FromHoaError;

    fn try_from(hoa_aut: HoaAutomaton) -> Result<Self, FromHoaError> {
        debug!("Parsing parity condition from HOA automaton");
        let (num_acceptance_sets, _acceptance_condition) = hoa_aut.acceptance();
        // For now we assume that everything is in min even format
        // TODO: support max even, min odd and max odd

        // verify that the acceptance name is parity and min even (for now)
        match hoa_aut.acceptance_name() {
            Some((&hoars::AcceptanceName::Parity, info)) => {
                if info[0..=1]
                    != [
                        AcceptanceInfo::Identifier("min".to_string()),
                        AcceptanceInfo::Identifier("even".to_string()),
                    ]
                {
                    return Err(FromHoaError::UnsupportedAcceptanceCondition);
                }
                trace!("Acceptance name is parity min even");
            }
            None => {
                info!("No acceptance name specified, assuming parity min even");
            }
            _ => {
                return Err(FromHoaError::UnsupportedAcceptanceCondition);
            }
        }

        // TODO: verify that the acceptance condition is fitting

        let mut out = std::iter::repeat(Set::new())
            .take(num_acceptance_sets)
            .collect::<Vec<_>>();

        for state in hoa_aut.body().iter() {
            let state_id = state.id();
            for edge in state.edges() {
                if edge.target().is_some() {
                    let acceptance = edge.acceptance_signature().get_singleton();
                    if let Some(Some(i)) = acceptance {
                        let label = edge.label().symbol();
                        out.get_mut(i as usize)
                            .expect("Acceptance set index out of bounds")
                            .insert((state_id, label));
                    } else if acceptance.is_none() {
                        return Err(FromHoaError::UnsupportedAcceptanceCondition);
                    }
                } else {
                    return Err(FromHoaError::UnsupportedBody);
                }
            }
        }
        Ok(ParityCondition(out))
    }
}

impl TryFrom<HoaAutomaton> for BuchiCondition<(Id, HoaSymbol)> {
    type Error = FromHoaError;

    fn try_from(hoa_aut: HoaAutomaton) -> Result<Self, FromHoaError> {
        let (num_acceptance_sets, acceptance_condition) = hoa_aut.acceptance();
        debug_assert!(
            num_acceptance_sets == 1,
            "In a Buchi condition there is exactly one acceptance set!"
        );

        let id = match acceptance_condition {
            AcceptanceCondition::Inf(id) => Ok(id),
            otherwise => Err(FromHoaError::ParseAcceptanceCondition(format!(
                "Expected Inf(<int>), got {}",
                otherwise
            ))),
        }?;
        assert_eq!(
            id,
            AcceptanceAtom::Positive(0),
            "In a Buchi condition the acceptance set is 0!"
        );

        let mut out = Set::new();

        for state in hoa_aut.body().iter() {
            let state_id = state.id();
            for edge in state.edges() {
                if edge.target().is_some() {
                    let acceptance = edge.acceptance_signature().get_singleton();
                    if let Some(Some(0)) = acceptance {
                        let label = edge.label().symbol();
                        out.insert((state_id, label));
                    } else if acceptance.is_none() {
                        return Err(FromHoaError::UnsupportedAcceptanceCondition);
                    }
                } else {
                    return Err(FromHoaError::UnsupportedBody);
                }
            }
        }
        Ok(BuchiCondition(out))
    }
}

impl<Acc: crate::AcceptanceCondition + TryFrom<HoaAutomaton, Error = FromHoaError>>
    TryFrom<HoaAutomaton> for Combined<Deterministic<Id, HoaSymbol>, Acc>
{
    type Error = FromHoaError;

    fn try_from(aut: HoaAutomaton) -> Result<Self, FromHoaError> {
        let version = aut.version();
        if version != "v1" {
            return Err(FromHoaError::UnsupportedVersion(version.to_string()));
        }

        let acceptance = Acc::try_from(aut.clone())?;

        let start = aut.start();
        if start.len() != 1 {
            // not deterministic, abort
            return Err(FromHoaError::UnsupportedBody);
        }
        let initial = start[0]
            .get_singleton()
            .ok_or(FromHoaError::UnsupportedBody)?;

        let mut ts = Deterministic::new();

        for state in aut.body().iter() {
            let state_id = state.id();
            ts.add_state(&state_id);
            for edge in state.edges() {
                if edge.target().is_some() {
                    let label = edge.label().symbol();
                    if let Some(target) = edge.target() {
                        ts.add_transition(state_id, label, target);
                    } else {
                        return Err(FromHoaError::UnsupportedBody);
                    }
                } else {
                    return Err(FromHoaError::UnsupportedBody);
                }
            }
        }

        Ok(Combined::from_parts(ts, initial, acceptance))
    }
}

/// Function for trying to parse a HOA automaton into a DBA.
pub fn parse_dba(hoa: &str) -> Result<Dba<Id, HoaSymbol>, FromHoaError> {
    let hoa_aut = HoaAutomaton::try_from(hoa)?;
    Combined::try_from(hoa_aut).map_err(|e| FromHoaError::ParseAcceptanceCondition(e.to_string()))
}

/// Function for trying to parse a HOA automaton into a DBA.
pub fn parse_dpa(hoa: &str) -> Result<Dpa<Id, HoaSymbol>, FromHoaError> {
    let hoa_aut = HoaAutomaton::try_from(hoa)?;
    Combined::try_from(hoa_aut).map_err(|e| FromHoaError::ParseAcceptanceCondition(e.to_string()))
}

/// Function for attempting to parse an omega automaton from a HOA string.
pub fn parse_hoa(hoa: &str) -> Result<OmegaAutomaton<u32, HoaSymbol>, FromHoaError> {
    let hoa_aut = HoaAutomaton::try_from(hoa)?;

    match hoa_aut.acceptance_name() {
        Some((&hoars::AcceptanceName::Buchi, _)) => {
            Combined::<_, BuchiCondition<_>>::try_from(hoa_aut)
                .map(|combined| combined.to_omega())
                .map_err(|e| FromHoaError::ParseAcceptanceCondition(e.to_string()))
        }
        Some((&hoars::AcceptanceName::Parity, _)) => {
            Combined::<_, ParityCondition<_>>::try_from(hoa_aut)
                .map(|combined| combined.to_omega())
                .map_err(|e| FromHoaError::ParseAcceptanceCondition(e.to_string()))
        }
        _ => Err(FromHoaError::UnsupportedAcceptanceCondition),
    }
}

#[cfg(test)]
mod tests {
    use hoars::HoaAutomaton;

    use crate::{combined::HoaDpa, BuchiCondition, Combined};

    #[test]
    fn hoa_parse_dba_test() {
        let contents = r#"HOA: v1
             AP: 1 "a"
             States: 3
             Start: 0
             acc-name: Buchi
             Acceptance: 1 Inf(0)
             --BODY--
             State: 0
              [0] 1
              [!0]  2
             State: 1 /* former state 0 */
              [0] 1  {0}
              [!0] 2 {0}
             State: 2  /* former state 1 */
              [0] 1
              [!0] 2
             --END--
             "#;
        let hoa_aut = HoaAutomaton::try_from(contents);
        let aut: Result<Combined<_, BuchiCondition<_>>, _> =
            super::Combined::try_from(hoa_aut.unwrap());
        println!("{}", aut.unwrap());
    }

    #[test]
    fn hoa_parse_dpa_test() {
        let hoa_dpa = r#"HOA: v1
        States: 3
        Start: 0
        AP: 2 "a" "b"
        acc-name: parity min even 3
        Acceptance: 3 Inf(0) | (Fin(1) & Inf(2))
        properties: trans-labels explicit-labels trans-acc complete
        properties: deterministic
        --BODY--
        State: 0
        [0] 1
        [!0] 0 {2}
        State: 1
        [t] 2 {1}
        State: 2
        [0] 1 {0}
        [!0] 2
        --END--"#;
        let hoa_aut = HoaAutomaton::try_from(hoa_dpa).expect("Could not parse HOA automaton");
        let aut = HoaDpa::try_from(hoa_aut);
        println!("{}", aut.unwrap());
    }
}
