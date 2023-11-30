use std::collections::BTreeSet;

use crate::{
    alphabet::{ExpressionOf, SymbolOf},
    length::RawPosition,
    ts::{EdgeColor, Path},
    Length,
};

use crate::ts::{IsTransition, TransitionSystem};

use super::{partial::Partial, successful::Successful};

/// A walker that can traverse a transition system for some given input word.
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Walker<Ts: TransitionSystem, R> {
    ts: Ts,
    word: R,
    position: usize,
    seen: BTreeSet<(RawPosition, Ts::StateIndex)>,
    seq: Path<Ts::Alphabet, Ts::StateIndex>,
}

/// Type alias for the result of a run.
pub type RunResult<Ts, R> = Result<Successful<R, Ts>, Partial<R, Ts>>;

/// Disambiguates between the different possible outcomes of a single step in the automaton.
pub enum WalkerStep<Ts: TransitionSystem> {
    /// A transition was taken.
    Transition((ExpressionOf<Ts>, Ts::StateIndex, EdgeColor<Ts>)),
    /// A missing transition was encountered.
    Missing(Ts::StateIndex, SymbolOf<Ts>),
    /// A cycle was entered.
    Cycle,
    /// The end of the word was reached.
    End,
}

impl<Ts: TransitionSystem> WalkerStep<Ts> {
    fn is_successful(&self) -> bool {
        matches!(self, Self::End) || matches!(self, Self::Cycle)
    }
}

impl<Ts: TransitionSystem, R: Word<Symbol = SymbolOf<Ts>>> Walker<Ts, R> {
    /// Creates a new walker for the given word, transition system and starting point/origin.
    pub fn new(word: R, ts: Ts, origin: Ts::StateIndex) -> Self {
        Self {
            ts,
            word,
            position: 0,
            seen: BTreeSet::new(),
            seq: Path::empty(origin),
        }
    }

    /// Takes transitions until the end of the word is reached or a cycle is entered and returns the
    /// [`RunResult`] of the run.
    pub fn result(mut self) -> RunResult<Ts, R> {
        loop {
            match self.take_transition() {
                WalkerStep::Transition(_) => continue,
                WalkerStep::Missing(_, _) => {
                    return Err(Partial::new(self.word, self.ts, self.position, self.seq))
                }
                WalkerStep::End => return Ok(Successful::new(self.word, self.ts, None, self.seq)),
                WalkerStep::Cycle => {
                    let loop_index = self
                        .word
                        .length()
                        .calculate_raw_position(self.position)
                        .map(|pos| pos.position());
                    return Ok(Successful::new(self.word, self.ts, loop_index, self.seq));
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

            if let Some(transition) = self.seq.extend_in(&self.ts, symbol) {
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
            WalkerStep::Transition(t) => Some(t),
            _ => None,
        }
    }

    fn current_sym(&self) -> Option<SymbolOf<Ts>> {
        self.word.nth(self.position)
    }
}
