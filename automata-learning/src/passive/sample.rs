use std::{cell::RefCell, collections::BTreeSet, hash::Hash};

use automata::{
    run::Evaluate,
    ts::{Bfs, HasInput, HasStates, IntoTransitions, TransitionOf, Trivial, Visitor},
    words::{IsFinite, IsInfinite},
    Acceptor, Class, FiniteKind, Pointed, RightCongruence, Set, State, Str, Subword, Successor,
    Symbol, TransitionSystem, UltimatelyPeriodicWord, Word, DFA,
};
use itertools::Itertools;
use tracing::trace;

use crate::glerc::{glerc, ConflictConstraint};

pub type OmegaSample<S> = Sample<UltimatelyPeriodicWord<S>>;
pub type FiniteSample<S> = Sample<Str<S>>;

/// Represents a finite sample, which is a pair of positive and negative instances.
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Sample<W: Word> {
    pub alphabet: BTreeSet<W::S>,
    pub positive: Set<W>,
    pub negative: Set<W>,
}

impl<W: Word> PartialEq for Sample<W> {
    fn eq(&self, other: &Self) -> bool {
        self.positive == other.positive && self.negative == other.negative
    }
}

impl<W: Word> Eq for Sample<W> {}

impl<W: Word> Sample<W> {
    /// Creates a new sample from the given data.
    pub fn from_parts(positive: Set<W>, negative: Set<W>) -> Self
    where
        W: Subword,
    {
        let alphabet = positive
            .iter()
            .chain(negative.iter())
            .flat_map(|w| w.alphabet())
            .collect();
        Self {
            positive,
            negative,
            alphabet,
        }
    }

    /// Returns the number of elements in the sample.
    pub fn len(&self) -> usize {
        self.positive.len() + self.negative.len()
    }

    /// Checks if the sample is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Creates a new sample from two iterators.
    pub fn from_iters<I, J>(positive: I, negative: J) -> Self
    where
        I: IntoIterator<Item = W>,
        J: IntoIterator<Item = W>,
    {
        let positive: Set<W> = positive.into_iter().collect();
        let negative: Set<W> = negative.into_iter().collect();
        let alphabet = positive
            .iter()
            .chain(negative.iter())
            .flat_map(|w| w.alphabet())
            .collect();
        Self {
            positive,
            negative,
            alphabet,
        }
    }

    /// Iterates over all elements in the sample.
    pub fn iter(&self) -> impl Iterator<Item = &W> {
        self.positive.iter().chain(self.negative.iter())
    }

    /// Iterates just over the positive instances.
    pub fn positive_iter(&self) -> impl Iterator<Item = &W> {
        self.positive.iter()
    }

    /// Iterates just over the negative instances.
    pub fn negative_iter(&self) -> impl Iterator<Item = &W> {
        self.negative.iter()
    }

    /// Iterates over all elements in the sample.
    pub fn annotated_iter(&self) -> impl Iterator<Item = (bool, &W)> {
        self.positive
            .iter()
            .map(|w| (true, w))
            .chain(self.negative.iter().map(|w| (false, w)))
    }
}

/// A helper struct which can be used to construct an acceptor for the prefixes of a
/// set of ultimately periodic words.
#[derive(Debug, Clone)]
pub struct Prefixes<'a, S: Symbol, W> {
    alphabet: Set<S>,
    set: &'a Set<W>,
    sink: RefCell<Option<Class<S>>>,
}

impl<'a, S: Symbol, W: Subword<S = S>> Prefixes<'a, S, W> {
    /// Creates a new instance of [`Prefixes`] for the given set of ultimately periodic words.
    pub fn new(set: &'a Set<W>) -> Self {
        let alphabet = set.iter().flat_map(|w| w.alphabet()).collect();

        Self {
            alphabet,
            set,
            sink: RefCell::new(None),
        }
    }

    fn find_words_with_prefix(&self, class: &Class<S>) -> Vec<&W> {
        self.set
            .iter()
            .filter(|w| w.has_finite_prefix(&class.0))
            .collect()
    }

    fn sink_or(&self, class: &Class<S>) -> Class<S> {
        self.sink
            .borrow_mut()
            .get_or_insert_with(|| class.clone())
            .clone()
    }

    fn is_sink(&self, class: &Class<S>) -> bool {
        self.sink.borrow().as_ref() == Some(class)
    }
}

impl<'a, S: Symbol, W: Subword<S = S>> HasStates for Prefixes<'a, S, W> {
    type Q = Class<S>;

    fn contains_state<X: std::borrow::Borrow<Self::Q>>(&self, state: X) -> bool {
        self.set
            .iter()
            .any(|word| word.has_finite_prefix(state.borrow().raw()))
    }
}

impl<'a, S: Symbol, W: Subword<S = S>> HasInput for Prefixes<'a, S, W> {
    type Sigma = S;

    type Input<'me> = itertools::Unique<std::collections::hash_set::Iter<'me, S>>
    where Self:'me;

    fn input_alphabet(&self) -> Self::Input<'_> {
        self.alphabet.iter().unique()
    }
}

impl<'a, S: Symbol> Successor for Prefixes<'a, S, Str<S>> {
    fn successor<X: std::borrow::Borrow<Self::Q>, Y: std::borrow::Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q> {
        let source = from.borrow();
        let sym = on.borrow();
        let successor = source + sym;
        trace!(
            "Computing successor of {} on {}, candidate is {}",
            source,
            sym,
            successor
        );

        let words_with_prefix = self.find_words_with_prefix(&successor);
        if words_with_prefix.is_empty() {
            trace!("No words with prefix {}!", successor);
            return Some(self.sink_or(&successor));
        }
        let count_words_with_prefix = words_with_prefix.len();
        trace!(
            "Found {} words with prefix {}",
            count_words_with_prefix,
            successor
        );

        debug_assert!(
            !words_with_prefix.is_empty(),
            "must have at least one word with prefix"
        );
        Some(successor)
    }
}

impl<'a, S: Symbol> Successor for Prefixes<'a, S, UltimatelyPeriodicWord<S>> {
    fn successor<X: std::borrow::Borrow<Self::Q>, Y: std::borrow::Borrow<Self::Sigma>>(
        &self,
        from: X,
        on: Y,
    ) -> Option<Self::Q> {
        let source = from.borrow();
        let sym = on.borrow();
        let successor = source + sym;
        // trace!(
        //     "Computing successor of {} on {}, candidate is {}",
        //     source,
        //     sym,
        //     successor
        // );

        let words_with_prefix = self.find_words_with_prefix(&successor);
        if words_with_prefix.is_empty() {
            // trace!("No words with prefix {}!", successor);
            return Some(self.sink_or(&successor));
        }
        let count_words_with_prefix = words_with_prefix.len();
        // trace!(
        //     "Found {} words with prefix {}",
        //     count_words_with_prefix,
        //     successor
        // );

        if words_with_prefix.len() > 1 {
            Some(successor)
        } else {
            assert!(words_with_prefix.len() == 1);
            let word = words_with_prefix[0];

            if word.recur_length() > successor.length() {
                return Some(successor);
            }

            let base = successor.prefix(successor.length() - word.recur_length());

            let words_with_prefix_for_base = self.find_words_with_prefix(&base);
            debug_assert!(!words_with_prefix_for_base.is_empty(), "Cannot happen!");

            if words_with_prefix_for_base.len() == 1 {
                Some(base)
            } else if !words_with_prefix_for_base.is_empty() {
                Some(successor)
            } else {
                None
            }
        }
    }
}

impl<'a, S: Symbol, W: Subword<S = S>> Pointed for Prefixes<'a, S, W>
where
    Prefixes<'a, S, W>: automata::Successor<Q = Class<S>>,
{
    fn initial(&self) -> Self::Q {
        Class::epsilon()
    }
}

fn build_prefix_dfa_finite<S: Symbol>(source: &Set<Str<S>>) -> DFA<Class<S>, S> where {
    let prefixes = Prefixes::new(source);
    let ts: TransitionSystem<_, S> = Bfs::new(&prefixes).iter().collect();
    let accepting: automata::output::Mapping<Class<S>, bool> = ts
        .states()
        .map(|q| (q.clone(), !prefixes.is_sink(q) && source.contains(q.raw())))
        .collect();
    DFA::from_parts(ts, Class::epsilon(), accepting)
}

fn build_prefix_dfa_infinite<S: Symbol>(
    source: &Set<UltimatelyPeriodicWord<S>>,
) -> DFA<Class<S>, S> {
    let pref = Prefixes::new(source);
    let ts: TransitionSystem<_, S> = Bfs::new(&pref).iter().collect();
    let accepting: automata::output::Mapping<Class<S>, bool> =
        ts.states().map(|q| (q.clone(), !pref.is_sink(q))).collect();
    DFA::from_parts(ts, Class::epsilon(), accepting)
}

type FiniteSampleInduced<'a, S> = (Vec<(&'a Str<S>, Class<S>)>, Vec<(&'a Str<S>, Class<S>)>);
type OmegaSampleInduced<'a, S> = (
    Vec<(
        &'a UltimatelyPeriodicWord<S>,
        Set<TransitionOf<RightCongruence<S>>>,
    )>,
    Vec<(
        &'a UltimatelyPeriodicWord<S>,
        Set<TransitionOf<RightCongruence<S>>>,
    )>,
);

impl<S: Symbol> OmegaSample<S> {
    fn alphabet(&self) -> &BTreeSet<S> {
        &self.alphabet
    }

    /// Returns the maximum length of the base prefix of any word in the sample.
    pub fn max_base_len(&self) -> usize {
        self.iter().map(|w| w.base_length()).max().unwrap_or(0)
    }

    /// Returns the maximum loop length of any word in the sample.
    pub fn max_recur_len(&self) -> usize {
        self.iter().map(|w| w.recur_length()).max().unwrap_or(0)
    }

    pub fn underlying_right_congruence(&self) -> RightCongruence<S> {
        let fallback = Prefixes::new(&self.positive).bfs().iter().collect();
        glerc(
            ConflictConstraint::mn_from_omega_sample(self),
            self.alphabet().clone(),
            fallback,
        )
    }

    /// Builds a [`DFA`] that accepts all finite words which are a prefix of some positive
    /// sample word.
    pub fn positive_prefixes(&self) -> DFA<Class<S>, S> {
        build_prefix_dfa_infinite(&self.positive)
    }

    /// Does the same as [`positive_prefixes`], but for negative sample words.
    pub fn negative_prefixes(&self) -> DFA<Class<S>, S> {
        build_prefix_dfa_infinite(&self.negative)
    }

    /// Computes what the positive sample words induce in `cong`.
    pub fn induced(&self, cong: &RightCongruence<S>) -> OmegaSampleInduced<'_, S> {
        (
            self.positive_iter()
                .filter_map(|word| {
                    let ind = cong.run(word).evaluate().ok()?;
                    Some((word, ind))
                })
                .collect(),
            self.negative_iter()
                .filter_map(|word| {
                    let ind = cong.run(word).evaluate().ok()?;
                    Some((word, ind))
                })
                .collect(),
        )
    }

    /// Constructs a default structure for `self`. In this case, we unroll all positive sample words far
    /// enough that they separate from each other and we attach loops to each leaf state.
    pub fn default_structure(&self) -> RightCongruence<S> {
        build_prefix_dfa_infinite(&self.positive).into_congruence()
    }
}

impl<S: Symbol> FiniteSample<S> {
    /// Builds a [`DFA`] that accepts all finite words which are a prefix of some positive
    /// sample word.
    pub fn positive_prefixes(&self) -> DFA<Class<S>, S> {
        build_prefix_dfa_finite(&self.positive)
    }

    /// Does the same as [`positive_prefixes`], but for negative sample words.
    pub fn negative_prefixes(&self) -> DFA<Class<S>, S> {
        build_prefix_dfa_finite(&self.negative)
    }

    /// Computes what is induced by the finite sample, that is the function runs
    /// each positive word and collects what it induces in `cong`.
    pub fn induced(&self, cong: &RightCongruence<S>) -> FiniteSampleInduced<'_, S> {
        (
            self.positive_iter()
                .filter_map(|pos| cong.run(pos).evaluate().ok().map(|ind| (pos, ind)))
                .collect(),
            self.negative_iter()
                .filter_map(|neg| cong.run(neg).evaluate().ok().map(|ind| (neg, ind)))
                .collect(),
        )
    }

    /// Returns a default structure for `self`. In this case, that is a prefix tree
    /// which naturally separates all positive words from each negative word.
    pub fn default_structure(&self) -> RightCongruence<S> {
        build_prefix_dfa_finite(&self.positive).into_congruence()
    }

    pub fn consistent_with<A: Acceptor<Word = Str<S>>>(&self, dfa: &A) -> bool {
        self.positive_iter().all(|pos| dfa.accepts(pos))
            && self.negative_iter().all(|neg| !dfa.accepts(neg))
    }
}

#[cfg(test)]
mod tests {
    use automata::{
        ts::{Bfs, Visitor},
        upw, Class, Set, UltimatelyPeriodicWord, DFA,
    };
    use tracing::trace;

    use super::{build_prefix_dfa_infinite, Prefixes};

    #[test]
    #[tracing_test::traced_test]
    fn set_prefixes_automaton() {
        trace!("set_prefixes_automaton");
        todo!()
        // let words = Set::from_iter([upw!("a"), upw!("b"), upw!("ab")]);
        // let prefixes = build_prefix_dfa(&words);

        // for p in ["", "a", "aaaaaa", "b", "bbbbb", "ababababa"] {
        //     assert!(prefixes.accepts(p));
        // }
        // for n in [
        //     "ba",
        //     "aaaaaaaaabaaaaaa",
        //     "aaaaaaaaaaaab",
        //     "bbbbbababababaababa",
        // ] {
        //     assert!(!prefixes.accepts(n));
        // }
    }
}
