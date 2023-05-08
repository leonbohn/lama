use std::{
    borrow::Borrow,
    collections::{BTreeSet, VecDeque},
    fmt::Display,
    ops::{Add, AddAssign},
};

use itertools::Itertools;

use crate::{
    ts::{HasInput, HasStates, StateOf},
    words::SymbolIterable,
    Set, StateIndex, Str, Successor, Symbol, Transition, Trigger,
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
        for x in self.states.iter().zip_longest(self.label.symbol_iter()) {
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
    pub fn empty(from: Q) -> Self {
        Self {
            states: vec![from],
            label: Str::epsilon(),
        }
    }

    /// Creates a new path from the given sequence of states and symbols.
    pub fn new<I: IntoIterator<Item = Q>, J: IntoIterator<Item = S>>(states: I, label: J) -> Self {
        let states = states.into_iter().collect_vec();
        let label = Str::from_iter(label);
        debug_assert_eq!(states.len(), label.len() + 1);
        debug_assert!(!states.is_empty());
        Self { states, label }
    }

    /// Returns a reference to the sequence of states that make up the path.
    /// This sequence is never empty.
    pub fn states(&self) -> &[Q] {
        debug_assert!(!self.states.is_empty());
        debug_assert_eq!(self.states.len(), self.label.len() + 1);
        &self.states
    }

    /// Returns the label of the path, i.e. the sequence of symbols that make up the path.
    /// The label is empty iff the path is empty.
    pub fn label(&self) -> &Str<S> {
        debug_assert_eq!(self.states.len(), self.label.len() + 1);
        &self.label
    }

    /// Returns the state at which the path ends. We consider only non-empty paths,
    /// so this always exists.
    pub fn reached(&self) -> &Q {
        self.states
            .last()
            .expect("We consider only non-empty paths!")
    }

    pub fn len(&self) -> usize {
        self.label.len()
    }

    /// Returns the state at which the path starts. We consider only non-empty paths,
    /// so this always exists.
    pub fn origin(&self) -> &Q {
        self.states
            .first()
            .expect("We consider only non-empty paths!")
    }

    /// Returns the last letter of the path, if it exists.
    pub fn last_letter(&self) -> Option<&S> {
        self.label.last()
    }

    /// Returns true iff the path is trivial, i.e. it contains of a single state and its label is empty.
    pub fn is_trivial(&self) -> bool {
        self.label.is_empty()
    }

    /// Checks whether the path is a cycle, meaning its origin and reached state are the same.
    /// Note, that a path of length 1 is not considered a cycle, so the label sequence must
    /// be non-empty.
    pub fn is_cycle(&self) -> bool {
        !self.is_trivial() && self.origin() == self.reached()
    }

    /// Checks whether the path is an elementary cycle, meaning it is a cycle and the origin
    /// state occurs exactly twice in the path, once at the start and once at the end.
    pub fn is_elementary_cycle(&self) -> bool {
        self.is_cycle()
            && self
                .states
                .iter()
                .skip(1)
                // skip the first state, ensure that each other state appears precisely once
                .try_fold(Set::new(), |mut acc, state| {
                    if acc.contains(state) {
                        // already seen, so not an elementary cycle
                        Err(())
                    } else {
                        acc.insert(state);
                        Ok(acc)
                    }
                })
                .is_ok()
    }

    /// Checks whether the path exists in the transition system `ts`,
    pub fn exists_in<TS: Successor<Q = Q>>(&self, ts: TS) -> bool {
        let mut it = self.states.iter();
        let initial = ts.contains_state(it.next().expect("Must exist"));
        it.fold(initial, |acc, state| acc && ts.contains_state(state))
    }
}

impl<Q: StateIndex, S: Symbol> Extend<(Q, S)> for Path<Q, S> {
    fn extend<T: IntoIterator<Item = (Q, S)>>(&mut self, iter: T) {
        for (state, symbol) in iter.into_iter() {
            self.states.push(state);
            self.label.push_back(symbol);
        }
    }
}

impl<Q: StateIndex, S: Symbol> Extend<(Q, S, Q)> for Path<Q, S> {
    fn extend<T: IntoIterator<Item = (Q, S, Q)>>(&mut self, iter: T) {
        for (source, symbol, target) in iter.into_iter() {
            assert!(self.reached() == &source, "Path must be continuous");
            self.states.push(target);
            self.label.push_back(symbol);
        }
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

impl<T: Transition> AddAssign<T> for Path<T::Q, T::S> {
    fn add_assign(&mut self, rhs: T) {
        assert!(self.reached() == rhs.source(), "Path must be continuous");
        self.states.push(rhs.target().clone());
        self.label.push_back(rhs.sym());
    }
}

impl<T: Transition> From<Vec<T>> for Path<T::Q, T::S> {
    fn from(value: Vec<T>) -> Self {
        assert!(
            !value.is_empty(),
            "Cannot create path from empty sequence of transitions"
        );
        let mut states = Vec::with_capacity(value.len() + 1);
        let mut label = Str::epsilon();
        let mut i = 0;
        let len = value.len();
        for transition in value {
            states.pop();
            states.push(transition.source().clone());
            label.push_back(transition.sym());
            todo!("Test that this actually makes sense.");
            if i == len - 1 {
                states.push(transition.target().clone());
            }
        }
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

/// A macro for constructing an ultimately periodic word from string(s).
#[macro_export]
macro_rules! path {
    ($state:expr) => {
        $crate::ts::visit::Path::new(vec![$state], vec![])
    };
    ($($state:expr)+ , $($sym:expr)+) => {
        $crate::ts::visit::Path::new(vec![$($state),+], vec![$($sym),+])
    };
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{
        ts::{visit::Path, Visitor},
        Successor, TransitionSystem,
    };

    #[test]
    fn path_and_macro() {
        assert!(!path!(0 1 2 1, 'a' 'a' 'a').is_cycle());
        assert!(path!(1 2 1, 'a' 'a').is_elementary_cycle())
    }

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
