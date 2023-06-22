use std::collections::BTreeSet;

use tracing::trace;

use crate::{
    alphabet::{HasAlphabet, SymbolOf},
    length::RawPosition,
    ts::{Path, StateIndex, Transition, TransitionSystem},
    word::Rawpresentation,
    Length, Word,
};

use super::{Partial, Successful, Successor};

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Walker<'a, 'b, Ts: Successor, R> {
    ts: &'a Ts,
    word: &'b R,
    position: usize,
    seen: BTreeSet<(RawPosition, StateIndex)>,
    seq: Path<'a, Ts::Alphabet, Ts::StateColor, Ts::EdgeColor>,
}

pub type RunResult<'a, 'b, Ts, R> = Result<Successful<'a, 'b, R, Ts>, Partial<'a, 'b, R, Ts>>;

pub enum WalkerStep<'a, Ts: Successor> {
    Transition(Transition<'a, SymbolOf<Ts>, Ts::EdgeColor>),
    Missing(StateIndex, SymbolOf<Ts>),
    Cycle,
    End,
}

impl<'a, Ts: Successor> WalkerStep<'a, Ts> {
    fn is_successful(&self) -> bool {
        matches!(self, Self::End) || matches!(self, Self::Cycle)
    }
}

impl<'a, 'b, Ts: Successor, R: Word<Symbol = SymbolOf<Ts>>> Walker<'a, 'b, Ts, R> {
    pub fn new(word: &'b R, ts: &'a Ts, origin: StateIndex) -> Self {
        Self {
            ts,
            word,
            position: 0,
            seen: BTreeSet::new(),
            seq: Path::empty(origin, ts.state_color(origin)),
        }
    }

    pub fn result(mut self) -> RunResult<'a, 'b, Ts, R> {
        self.take_all_steps();

        if self.is_successful() {
            Ok(Successful::new(self.word, self.ts, self.seq))
        } else {
            Err(Partial::new(self.word, self.ts, self.position, self.seq))
        }
    }

    fn is_successful(&mut self) -> bool {
        self.take_transition().is_successful()
    }

    fn take_transition(&mut self) -> WalkerStep<'a, Ts> {
        if let Some(symbol) = self.word.get(self.position) {
            let rawposition = self
                .word
                .length()
                .calculate_raw_position(self.position)
                .expect("rawposition must exist as we were able to get the symbol");

            if !self.seen.insert((rawposition, self.seq.reached())) {
                return WalkerStep::Cycle;
            }

            if let Some(transition) = self.seq.extend_in(self.ts, symbol) {
                WalkerStep::Transition(transition)
            } else {
                WalkerStep::Missing(self.seq.reached(), symbol)
            }
        } else {
            WalkerStep::End
        }
    }

    pub fn step(&mut self) -> Option<Transition<'a, SymbolOf<Ts>, Ts::EdgeColor>> {
        match self.take_transition() {
            WalkerStep::Transition(t) => {
                trace!("Took transition {:?} at position {}", t, self.position);
                self.position += 1;
                Some(t)
            }
            _ => None,
        }
    }

    pub fn take_all_steps(&mut self) {
        while let Some(transition) = self.step() {}
    }

    fn current_sym(&self) -> Option<SymbolOf<Ts>> {
        self.word.get(self.position)
    }
}
