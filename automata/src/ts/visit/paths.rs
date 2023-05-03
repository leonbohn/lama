use std::{
    borrow::Borrow,
    collections::{BTreeSet, VecDeque},
    fmt::Display,
    ops::Add,
};

use itertools::Itertools;

use crate::{
    ts::{HasInput, HasStates, StateOf},
    words::SymbolIterable,
    StateIndex, Str, Successor, Symbol, Trigger,
};

use super::Visitor;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Path<Q, S> {
    states: Vec<Q>,
    label: Str<S>,
}

impl<Q: Display, S: Symbol + Display> std::fmt::Display for Path<Q, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Path: [ ")?;
        for x in self.states.iter().zip_longest(self.label.iter()) {
            match x {
                itertools::EitherOrBoth::Both(q, s) => write!(f, "{}-{}->", q, s)?,
                itertools::EitherOrBoth::Left(q) => write!(f, "{} ]", q)?,
                itertools::EitherOrBoth::Right(s) => unreachable!(),
            }
        }
        Ok(())
    }
}

impl<Q: StateIndex, S: Symbol> Path<Q, S> {
    pub fn new<I: IntoIterator<Item = Q>, J: IntoIterator<Item = S>>(states: I, label: J) -> Self {
        let states = states.into_iter().collect_vec();
        let label = Str::from_iter(label);
        debug_assert_eq!(states.len(), label.len() + 1);
        Self { states, label }
    }

    pub fn states(&self) -> &[Q] {
        debug_assert_eq!(self.states.len(), self.label.len() + 1);
        &self.states
    }

    pub fn label(&self) -> &Str<S> {
        debug_assert_eq!(self.states.len(), self.label.len() + 1);
        &self.label
    }

    pub fn extend(&mut self, state: Q, symbol: S) {
        self.states.push(state);
        self.label.push_back(symbol);
    }

    pub fn reached(&self) -> &Q {
        self.states
            .last()
            .expect("We consider only non-empty paths!")
    }

    pub fn origin(&self) -> &Q {
        self.states
            .first()
            .expect("We consider only non-empty paths!")
    }

    pub fn last_letter(&self) -> Option<&S> {
        self.label.last()
    }

    pub fn is_cycle(&self) -> bool {
        self.origin() == self.reached()
    }
}

impl<T: Trigger> Add<T> for &Path<T::Q, T::S> {
    type Output = Path<T::Q, T::S>;

    fn add(self, rhs: T) -> Self::Output {
        let mut states = self.states.clone();
        let mut label = self.label.clone();
        states.push(rhs.source().clone());
        label.push_back(rhs.sym());
        Path { states, label }
    }
}

type PathFor<TS> = Path<<TS as HasStates>::Q, <TS as HasInput>::Sigma>;
type PathQueue<TS> = VecDeque<(
    Path<<TS as HasStates>::Q, <TS as HasInput>::Sigma>,
    <TS as HasInput>::Sigma,
)>;
pub struct LLexPaths<TS: Successor> {
    ts: TS,
    alphabet: BTreeSet<TS::Sigma>,
    queue: PathQueue<TS>,
}

impl<TS> LLexPaths<TS>
where
    TS: Successor,
{
    pub fn new_from<X: Borrow<TS::Q>>(ts: TS, origin: X) -> Self {
        let alphabet: BTreeSet<_> = ts.input_alphabet().cloned().collect();
        Self {
            ts,
            queue: alphabet
                .iter()
                .map(|sym| {
                    (
                        Path::new(vec![origin.borrow().clone()], vec![]),
                        sym.clone(),
                    )
                })
                .collect(),
            alphabet,
        }
    }
}

impl<TS: Successor> Visitor for LLexPaths<TS> {
    type Place = PathFor<TS>;

    fn visit_next(&mut self) -> Option<Self::Place> {
        while let Some((path, sym)) = self.queue.pop_front() {
            if let Some(successor) = self.ts.successor(path.reached(), &sym) {
                let extended = path.add((successor, sym));
                for sym in &self.alphabet {
                    self.queue.push_back((extended.clone(), sym.clone()));
                }
                return Some(extended);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{
        ts::{visit::Path, Visitor},
        Successor, TransitionSystem,
    };

    #[test]
    fn simple_paths_test() {
        let ts = TransitionSystem::from_iter([
            (0, 'a', 1),
            (0, 'b', 2),
            (1, 'a', 2),
            (2, 'a', 1),
            (2, 'b', 2),
        ]);
        let mut it = ts.all_paths_from(0).iter();
        assert_eq!(it.next().unwrap(), Path::new([0, 1], ['a']),);
        assert_eq!(it.next().unwrap(), Path::new([0, 2], ['b']),);
        assert_eq!(it.next().unwrap(), Path::new([0, 1, 2], ['a', 'a']),);
        assert_eq!(it.next().unwrap(), Path::new([0, 2, 1], ['b', 'a']),);
        assert_eq!(it.next().unwrap(), Path::new([0, 2, 2], ['b', 'b']),);
        assert_eq!(it.next().unwrap(), Path::new([0, 1, 2, 1], ['a', 'a', 'a']),);
        assert_eq!(it.next().unwrap(), Path::new([0, 1, 2, 2], ['a', 'a', 'b']),);
    }
}
