use std::collections::BTreeSet;

use tracing::trace;

use crate::{
    alphabet::{ExpressionOf, HasAlphabet, SymbolOf},
    length::RawPosition,
    ts::{EdgeColor, Path, StateColor, StateIndex, Transition},
    Length, Word,
};

use super::{IsTransition, Partial, Successful, TransitionSystem};

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Walker<'a, 'b, Ts: TransitionSystem, R> {
    ts: &'a Ts,
    word: &'b R,
    position: usize,
    seen: BTreeSet<(RawPosition, Ts::StateIndex)>,
    seq: Path<Ts::Alphabet, Ts::StateIndex>,
}

pub type RunResult<'a, 'b, Ts, R> = Result<Successful<'a, 'b, R, Ts>, Partial<'a, 'b, R, Ts>>;

pub enum WalkerStep<Ts: TransitionSystem> {
    Transition((ExpressionOf<Ts>, Ts::StateIndex, EdgeColor<Ts>)),
    Missing(Ts::StateIndex, SymbolOf<Ts>),
    Cycle,
    End,
}

impl<Ts: TransitionSystem> WalkerStep<Ts> {
    fn is_successful(&self) -> bool {
        matches!(self, Self::End) || matches!(self, Self::Cycle)
    }
}

impl<'a, 'b, Ts: TransitionSystem, R: Word<Symbol = SymbolOf<Ts>>> Walker<'a, 'b, Ts, R> {
    pub fn new(word: &'b R, ts: &'a Ts, origin: Ts::StateIndex) -> Self {
        Self {
            ts,
            word,
            position: 0,
            seen: BTreeSet::new(),
            seq: Path::empty(origin),
        }
    }

    pub fn result(mut self) -> RunResult<'a, 'b, Ts, R> {
        loop {
            match self.take_transition() {
                WalkerStep::Transition(_) => continue,
                WalkerStep::Missing(_, _) => {
                    return Err(Partial::new(self.word, self.ts, self.position, self.seq))
                }
                WalkerStep::End => return Ok(Successful::new(self.word, self.ts, None, self.seq)),
                WalkerStep::Cycle => {
                    return Ok(Successful::new(
                        self.word,
                        self.ts,
                        self.word
                            .length()
                            .calculate_raw_position(self.position)
                            .map(|pos| pos.position()),
                        self.seq,
                    ))
                }
            }
        }
    }

    fn is_successful(&mut self) -> bool {
        self.take_transition().is_successful()
    }

    fn loop_index(&self) -> Option<usize> {
        self.raw_position().map(|pos| pos.position())
    }

    fn raw_position(&self) -> Option<RawPosition> {
        self.word.length().calculate_raw_position(self.position)
    }

    fn take_transition(&mut self) -> WalkerStep<Ts> {
        if let Some(symbol) = self.word.nth(self.position) {
            let rawposition = self
                .word
                .length()
                .calculate_raw_position(self.position)
                .expect("rawposition must exist as we were able to get the symbol");

            if !self.seen.insert((rawposition, self.seq.reached())) {
                return WalkerStep::Cycle;
            }

            if let Some(transition) = self.seq.extend_in(self.ts, symbol) {
                self.position += 1;
                WalkerStep::Transition(transition.into_tuple())
            } else {
                WalkerStep::Missing(self.seq.reached(), symbol)
            }
        } else {
            WalkerStep::End
        }
    }

    /// Takes a single step in the automaton.
    #[allow(clippy::type_complexity)]
    pub fn step(&mut self) -> Option<(ExpressionOf<Ts>, Ts::StateIndex, Ts::EdgeColor)> {
        match self.take_transition() {
            WalkerStep::Transition(t) => {
                trace!("Took transition {:?} at position {}", t, self.position);
                Some(t)
            }
            _ => None,
        }
    }

    fn current_sym(&self) -> Option<SymbolOf<Ts>> {
        self.word.nth(self.position)
    }
}
