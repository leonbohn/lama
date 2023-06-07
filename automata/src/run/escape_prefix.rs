use std::{
    cmp::Ordering,
    fmt::{Debug, Display},
};

use itertools::Itertools;

use crate::{Equivalent, State, Subword, Word};

/// An escape prefix for a transition system is a triple `(u, q, a)`, where `u` is a finite sequence of triggers for the transition system, `q` is a state of the transition system and `a` is a symbol such that:
/// - the last trigger in `u` brings the transition system into the state `q`
/// - no transition is defined for the symbol `a` in the state `q`.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct EscapePrefix<Q, W: Subword> {
    /// The prefix on which a run was possible, consists of state symbol pairs.
    pub prefix: Vec<(Q, W::S)>,
    /// The state at which the transition system is left.
    pub state: Q,
    /// The symbol on which the transition system is left.
    pub symbol: W::S,
    /// The remaining suffix of the word.
    pub suffix: W::SuffixType,
}

impl<Q, W> PartialOrd for EscapePrefix<Q, W>
where
    Q: State,
    W: Subword,
    W::S: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.prefix.len().partial_cmp(&other.prefix.len()) {
            Some(Ordering::Less) => Some(Ordering::Less),
            Some(Ordering::Greater) => Some(Ordering::Greater),
            _ => Some(
                self.prefix
                    .iter()
                    .zip(other.prefix.iter())
                    .find_map(|((_, a), (_, b))| {
                        let compared = a.partial_cmp(b);
                        if compared.is_none() || compared.unwrap() == Ordering::Equal {
                            None
                        } else {
                            compared
                        }
                    })
                    .unwrap_or_else(|| self.symbol.cmp(&other.symbol)),
            ),
        }
    }
}

impl<Q, W> Ord for EscapePrefix<Q, W>
where
    Q: State,
    W: Subword,
    W::S: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        match self.partial_cmp(other) {
            Some(o) => o,
            None => unreachable!("This must be a total order"),
        }
    }
}

impl<Q: Eq, W: Subword> Equivalent for EscapePrefix<Q, W> {
    fn equivalent(&self, other: &Self) -> bool {
        self.state == other.state && self.symbol == other.symbol && self.suffix == other.suffix
    }
}

impl<Q: State, W: Word + Subword> EscapePrefix<Q, W> {
    /// Creates a new escape prefix from the given prefix, state and symbol.
    pub fn new(word: &W, prefix: Vec<(Q, W::S)>, state: Q, symbol: W::S) -> Self {
        let length = prefix.len();
        Self {
            prefix,
            state,
            symbol,
            suffix: word.skip(length),
        }
    }

    /// Creates an owned trigger.
    pub fn trigger(&self) -> (Q, W::S) {
        (self.state.clone(), self.symbol.clone())
    }
}

impl<Q: Debug, W: Subword> Debug for EscapePrefix<Q, W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} then {:?} misses {} with rest {:?}",
            self.prefix
                .iter()
                .map(|(q, s)| format!("({:?},{:?})", q, s))
                .join(""),
            self.state,
            self.symbol,
            self.suffix
        )
    }
}

impl<Q: Display, W: Subword + Display> Display for EscapePrefix<Q, W>
where
    <W as Subword>::SuffixType: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} then {} misses {} with rest {}",
            self.prefix
                .iter()
                .map(|(q, s)| format!("({},{})", q, s))
                .join(""),
            self.state,
            self.symbol,
            self.suffix
        )
    }
}
