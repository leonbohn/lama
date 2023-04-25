use std::{fmt::Display, hash::Hash};

use hoars::{
    AcceptanceAtom, AcceptanceCondition, AcceptanceInfo, AcceptanceName, AcceptanceSignature, Edge,
    FromHoaError, HeaderItem, HoaAutomaton, HoaBool, HoaSymbol, Id, Label, LabelExpression, State,
    StateConjunction,
};
use tracing::{debug, info, trace};

use crate::{
    ts::{HasInput, HasStates},
    BuchiCondition, Combined, Dba, Deterministic, Dpa, Growable, HasAlphabet, Mapping,
    OmegaAutomaton, OmegaCondition, ParityCondition, Pointed, Set, Symbol, TransitionSystem,
};

pub trait ToHoaAcceptance {
    type Trigger;

    fn to_hoa_acceptance(&self) -> (Id, AcceptanceCondition, AcceptanceName, Vec<AcceptanceInfo>);

    fn acceptance_signature(&self, of: &Self::Trigger) -> AcceptanceSignature;
}

pub trait ToHoaLabel {
    fn to_hoa_symbol(&self, maximum: u32) -> Label;

    fn max_ap_index(&self) -> u32;
}

pub trait ToHoa {
    fn to_hoa(&self) -> HoaAutomaton;
}

impl ToHoaLabel for char {
    fn to_hoa_symbol(&self, maximum: u32) -> Label {
        let ascii_based_at_a = *self as u32 - 'a' as u32;
        Label(
            (0..maximum)
                .map(|i| {
                    if i == ascii_based_at_a {
                        LabelExpression::Integer(i)
                    } else {
                        LabelExpression::Not(Box::new(LabelExpression::Integer(i)))
                    }
                })
                .reduce(|acc, expr| LabelExpression::And(Box::new(acc), Box::new(expr)))
                .unwrap_or(LabelExpression::Boolean(HoaBool(true))),
        )
    }

    fn max_ap_index(&self) -> u32 {
        *self as u32 - 'a' as u32
    }
}

impl ToHoaLabel for HoaSymbol {
    fn to_hoa_symbol(&self, _maximum: u32) -> Label {
        self.0.to_label()
    }

    fn max_ap_index(&self) -> u32 {
        self.0.get_aps().cloned().max().unwrap_or(0)
    }
}

impl<X, Y> ToHoaAcceptance for OmegaCondition<(X, Y)>
where
    X: Display,
    Y: Display,
    (X, Y): PartialEq + Eq + std::hash::Hash + Clone,
{
    type Trigger = (X, Y);

    fn to_hoa_acceptance(&self) -> (Id, AcceptanceCondition, AcceptanceName, Vec<AcceptanceInfo>) {
        match self {
            OmegaCondition::Parity(parity) => (
                parity.priorities() as Id,
                AcceptanceCondition::parity(parity.priorities() as Id),
                // TODO: make this more generic
                AcceptanceName::Parity,
                vec![
                    AcceptanceInfo::identifier("min"),
                    AcceptanceInfo::identifier("even"),
                    AcceptanceInfo::integer(parity.priorities() as Id),
                ],
            ),
            OmegaCondition::Buchi(_) => (
                1,
                AcceptanceCondition::buchi(),
                AcceptanceName::Buchi,
                vec![AcceptanceInfo::integer(1)],
            ),
        }
    }

    fn acceptance_signature(&self, of: &Self::Trigger) -> AcceptanceSignature {
        match self {
            OmegaCondition::Parity(parity) => {
                AcceptanceSignature::from_singleton(parity.apply(of) as Id)
            }
            OmegaCondition::Buchi(buchi) => {
                if buchi.apply(of) {
                    AcceptanceSignature::from_singleton(0)
                } else {
                    AcceptanceSignature::empty()
                }
            }
        }
    }
}

impl<I, TS, Acc> ToHoa for Combined<TS, Acc>
where
    I: ToHoaLabel + Clone + Eq + Hash,
    TS: TransitionSystem<Sigma = I> + HasAlphabet,
    Acc: ToHoaAcceptance<Trigger = (TS::Q, TS::Sigma)>,
{
    fn to_hoa(&self) -> HoaAutomaton {
        let mut hoa = HoaAutomaton::default();
        let mut states = self.states().collect::<Vec<_>>();
        states.sort();

        let alphabet = self.input_alphabet().collect::<Set<_>>();

        hoa.add_header_item(HeaderItem::Version("v1".to_string()));
        hoa.add_header_item(HeaderItem::States(states.len() as Id));

        let (num_acceptance_sets, acceptance_condition, acceptance_name, acceptance_info) =
            self.acceptance().to_hoa_acceptance();
        hoa.add_header_item(HeaderItem::Acceptance(
            num_acceptance_sets,
            acceptance_condition,
        ));
        hoa.add_header_item(HeaderItem::AcceptanceName(acceptance_name, acceptance_info));

        let max_ap = alphabet
            .iter()
            .map(|symbol| symbol.max_ap_index())
            .max()
            .unwrap_or(0);

        let idx = states.iter().cloned().enumerate().collect::<Vec<_>>();

        for (i, state) in &idx {
            let mut edges = Vec::new();
            for sym in &alphabet {
                if let Some(target) = self.successor(state, sym) {
                    let target_id = idx.iter().find(|(_, s)| *s == &target).unwrap().0;
                    let edge = Edge::from_parts(
                        sym.to_hoa_symbol(max_ap),
                        StateConjunction::singleton(target_id as Id),
                        self.acceptance()
                            .acceptance_signature(&((*state).clone(), (*sym).clone())),
                    );
                    edges.push(edge);
                }
            }
            let state = State::from_parts(*i as Id, Some(format!("{:?}", state)), edges);
            hoa.add_state(state);
        }

        let initial_id = idx.iter().find(|(_, s)| *s == &self.initial()).unwrap().0;
        hoa.add_header_item(HeaderItem::Start(StateConjunction::singleton(
            initial_id as u32,
        )));

        // TODO: remove this stupid hack
        hoa.add_header_item(HeaderItem::AP(
            (0..=max_ap)
                .map(|i| ((i + 'a' as u32) as u8 as char).to_string())
                .collect(),
        ));

        hoa
    }
}

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
            return Err(FromHoaError::UnsupportedVersion(version));
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
    use super::ToHoa;
    use crate::{combined::HoaDpa, parse_dba, BuchiCondition, Combined};
    use hoars::HoaAutomaton;

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
        println!("HOA");
        let aut: Result<Combined<_, BuchiCondition<_>>, _> =
            super::Combined::try_from(hoa_aut.unwrap());
        println!("{}", aut.unwrap());
    }

    #[test]
    fn hoa_parse_unparse_test() {
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
        let dba = parse_dba(contents).unwrap();
        let hoa = dba.to_omega().to_hoa();
        println!("original:\n{}\nparsed:\n{}", contents, hoa);
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
