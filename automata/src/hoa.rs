use std::fmt::Display;

use hoars::{AcceptanceAtom, AcceptanceCondition, HoaAutomaton, HoaSymbol, Id};

use crate::{BuchiCondition, Combined, Deterministic, Growable, Set};

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

/// Represents the different types of error that can be encountered when parsing a [`HoaAutomaton`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FromHoaError {
    /// The version string does not match, we only support v1.
    UnsupportedVersion(String),
    UnsupportedAcceptanceCondition,
    ParseAcceptanceCondition(String),
    UnsupportedBody,
}

impl Display for FromHoaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FromHoaError::UnsupportedVersion(version) => {
                write!(f, "Unsupported HOA version ({})", version)
            }
            FromHoaError::UnsupportedAcceptanceCondition => {
                write!(f, "Unsupported acceptance condition")
            }
            FromHoaError::UnsupportedBody => write!(f, "Unsupported body"),
            FromHoaError::ParseAcceptanceCondition(message) => {
                write!(f, "Could not parse acceptance condition: {}", message)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use hoars::HoaAutomaton;

    use crate::{BuchiCondition, Combined};

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
}
