use std::ops::Deref;

use hoars::{HoaAutomaton, MAX_APS};

use crate::{
    automaton::{AcceptanceMask, OmegaAcceptanceCondition, OmegaAutomaton},
    hoa::HoaExpression,
    prelude::Initialized,
    ts::{Sproutable, NTS},
    TransitionSystem,
};

use super::HoaAlphabet;

/// Considers the given HOA string as a single automaton and tries to parse it into an
/// [`OmegaAutomaton`].
pub fn hoa_to_ts(hoa: &str) -> Vec<OmegaAutomaton<HoaAlphabet>> {
    let mut out = vec![];
    for hoa_aut in hoars::parse_hoa_automata(hoa) {
        match hoa_aut.try_into() {
            Ok(aut) => out.push(aut),
            Err(e) => tracing::warn!("Encountered parsing error {}", e),
        }
    }
    out
}

impl TryFrom<&hoars::Header> for OmegaAcceptanceCondition {
    type Error = String;

    fn try_from(value: &hoars::Header) -> Result<Self, Self::Error> {
        if !value.acceptance_name().is_parity() {
            return Err("Unhandled acceptance type".to_string());
        }
        Ok(OmegaAcceptanceCondition::Parity)
    }
}

impl TryFrom<HoaAutomaton> for OmegaAutomaton<HoaAlphabet> {
    type Error = String;
    fn try_from(value: HoaAutomaton) -> Result<Self, Self::Error> {
        let acc = value.header().try_into()?;
        let ts = hoa_automaton_to_nts(value);
        Ok(Self::new(ts, acc))
    }
}

/// Converts a [`HoaAutomaton`] into a [`NTS`] with the same semantics. This creates the appropriate
/// number of states and inserts transitions with the appropriate labels and colors.
pub fn hoa_automaton_to_nts(
    aut: HoaAutomaton,
) -> Initialized<NTS<HoaAlphabet, usize, AcceptanceMask>> {
    let aps = aut.num_aps();
    assert!(aps < MAX_APS);
    let mut ts = NTS::new_for_alphabet(HoaAlphabet::from_hoa_automaton(&aut));
    for (id, state) in aut.body().iter().enumerate() {
        assert_eq!(id, state.id() as usize);
        assert_eq!(id, ts.add_state(state.id() as usize));
    }
    for state in aut.body().iter() {
        for edge in state.edges() {
            let target = edge
                .state_conjunction()
                .get_singleton()
                .expect("Cannot yet deal with conjunctions of target states")
                as usize;
            let label = edge.label().deref().clone();
            let expr = HoaExpression::new(label, aps);

            let color: AcceptanceMask = edge.acceptance_signature().into();
            ts.add_edge(state.id() as usize, expr, target, color);
        }
    }

    let start = aut.start();
    assert_eq!(start.len(), 1);
    let initial = start[0]
        .get_singleton()
        .expect("Initial state must be a singleton") as usize;

    ts.with_initial(initial)
}
