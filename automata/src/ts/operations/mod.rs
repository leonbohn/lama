use std::{fmt::Display, marker::PhantomData};

use itertools::Itertools;

use crate::{
    alphabet::{Alphabet, Expression, ExpressionOf, HasAlphabet, Symbol, SymbolOf},
    Acceptor, Color, FiniteLength, Length, Transformer,
};

use super::{
    transition_system::IsTransition, Edge, EdgeColor, FiniteState, FiniteStatesIterType,
    HasFiniteStates, HasStates, IndexType, Pointed, StateColor, StateIndex, Transition,
    TransitionSystem,
};

mod map;
mod product;
mod restricted;

pub use map::*;
pub use product::*;
pub use restricted::*;

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{
        alphabet::Simple,
        ts::{
            finite::ReachedState,
            operations::product::{Product, ProductIndex},
            HasColorMut, HasMutableStates, Pointed, Sproutable, TransitionSystem,
        },
        Transformer,
    };

    #[test]
    fn product() {
        let mut dfa = crate::DFA::new(Simple::new(['a', 'b']));
        let s0 = dfa.initial();
        dfa.state_mut(s0).unwrap().set_color(true);
        let s1 = dfa.add_state(false);
        let _e0 = dfa.add_edge(s0, 'a', s1, ());
        let _e1 = dfa.add_edge(s0, 'b', s0, ());
        let _e2 = dfa.add_edge(s1, 'a', s1, ());
        let _e3 = dfa.add_edge(s1, 'b', s0, ());

        let mut dfb = crate::DFA::new(Simple::new(['a', 'b']));
        let s0 = dfb.initial();
        dfb.state_mut(s0).unwrap().set_color(true);
        let s1 = dfb.add_state(false);
        let _e0 = dfb.add_edge(s0, 'a', s1, ());
        let _e1 = dfb.add_edge(s0, 'b', s0, ());
        let _e2 = dfb.add_edge(s1, 'a', s1, ());
        let _e3 = dfb.add_edge(s1, 'b', s0, ());

        let xxx = dfa.ts_product(dfb);
        if let Some(ReachedState(q)) = xxx.induced(&"abb", ProductIndex(0, 0)) {}
        let c = xxx.transform("aa");

        let yyy = xxx.clone().map_colors(|(a, b)| a || b);
        let d = yyy.transform("aa");

        assert_eq!(c.0 || c.1, d);
        println!("{:?}", xxx.edges_from(xxx.initial()).unwrap().collect_vec());
    }
}
