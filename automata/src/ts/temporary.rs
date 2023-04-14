use crate::{Growable, Mapping, Set, TransitionSystem};

use super::SymbolOf;

pub struct Temporary<'ts, TS: TransitionSystem> {
    ts: &'ts TS,
    states: Set<TS::Q>,
    transitions: Mapping<(TS::Q, TS::S), TS::Q>,
}

impl<'ts, TS: TransitionSystem> Temporary<'ts, TS> {
    pub fn new(ts: &'ts TS) -> Self {
        Self {
            ts,
            states: Set::new(),
            transitions: Mapping::new(),
        }
    }
}

impl<'ts, TS: Growable + Clone> Temporary<'ts, TS> {
    pub fn permeate(self) -> TS {
        let mut ts = self.ts.clone();
        for state in self.states {
            ts.add_state(&state);
        }
        for ((from, on), to) in self.transitions {
            ts.add_transition(from, on, to);
        }
        ts
    }
}

impl<'ts, TS: TransitionSystem> TransitionSystem for Temporary<'ts, TS> {
    type Q = TS::Q;
    type S = TS::S;

    fn succ(&self, from: &Self::Q, on: &Self::S) -> Option<Self::Q> {
        self.transitions
            .get(&(from.clone(), on.clone()))
            .cloned()
            .or(self.ts.succ(from, on))
    }
}

impl<'ts, TS: Growable> Growable for Temporary<'ts, TS> {
    fn add_state(&mut self, state: &TS::Q) -> bool {
        self.states.insert(state.clone())
    }

    fn add_transition<X: std::borrow::Borrow<Self::Q>, Y: std::borrow::Borrow<Self::Q>>(
        &mut self,
        from: X,
        on: SymbolOf<Self>,
        to: Y,
    ) -> Option<Self::Q> {
        self.transitions
            .insert((from.borrow().clone(), on.clone()), to.borrow().clone());
        self.ts.succ(from.borrow(), &on)
    }
}
