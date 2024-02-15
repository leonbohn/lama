#![allow(missing_docs)]
use std::{
    collections::{BTreeMap, VecDeque},
    fmt::{Debug, Display},
    hash::Hash,
    marker::PhantomData,
};

use itertools::Itertools;

use crate::{prelude::*, Map, Void};

pub trait HasNeutral {
    fn neutral() -> Self;
}

impl HasNeutral for usize {
    fn neutral() -> Self {
        usize::MAX
    }
}
impl HasNeutral for bool {
    fn neutral() -> Self {
        false
    }
}
impl HasNeutral for () {
    fn neutral() -> Self {}
}
impl HasNeutral for Void {
    fn neutral() -> Self {
        Void
    }
}

/// Encapsulates the behavour of colors along run segments. This is what we are mainly
/// interested in and which we want to keep track of. As an example consider a run segment, where
/// each edge outputs an `usize`. If we are intersted in semantics of a parity automaton,
/// then we mainly want to know what the combined behaviour of the visited edges is.
/// As we use min-parity, this means we take the minimal value along all edges.
pub trait Accumulates: Sized + Clone + Eq + Hash + Ord {
    type X: Clone + Sized;
    /// Updates self with the received value.
    fn update(&mut self, other: &Self::X);
    /// The neutral element with regard to accumulation, which is `false` for booleans and the maximal
    /// `usize::MAX` for integers.
    fn neutral() -> Self;
    /// Accumulate an iterator into a single value.
    fn from_iter<'a, I: IntoIterator<Item = &'a Self::X>>(iter: I) -> Self
    where
        Self: 'a;
    fn from(x: Self::X) -> Self;
    fn from_or_neutral(x: Option<Self::X>) -> Self {
        match x {
            Some(x) => Self::from(x),
            None => Self::neutral(),
        }
    }
    fn result(&self) -> &Self::X;
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Replaces<X>(X);

impl<X: Clone + HasNeutral + Eq + Ord + Hash> Accumulates for Replaces<X> {
    type X = X;
    fn from(x: Self::X) -> Self {
        Self(x)
    }

    fn from_iter<'a, I: IntoIterator<Item = &'a Self::X>>(iter: I) -> Self
    where
        Self: 'a,
    {
        Self(
            iter.into_iter()
                .last()
                .expect("We assume the iterator to be non-empty")
                .clone(),
        )
    }
    fn update(&mut self, other: &Self::X) {
        self.0 = other.clone()
    }

    fn result(&self) -> &Self::X {
        &self.0
    }

    fn neutral() -> Self
    where
        Self::X: HasNeutral,
    {
        Self(X::neutral())
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Reduces<X>(X);

impl Accumulates for Reduces<usize> {
    type X = usize;

    fn update(&mut self, other: &Self::X) {
        self.0 = std::cmp::min(self.0, *other)
    }

    fn neutral() -> Self
    where
        Self::X: HasNeutral,
    {
        Self(Self::X::neutral())
    }

    fn from_iter<'a, I: IntoIterator<Item = &'a Self::X>>(iter: I) -> Self
    where
        Self: 'a,
    {
        Self(*iter.into_iter().min().unwrap())
    }

    fn result(&self) -> &Self::X {
        &self.0
    }

    fn from(x: Self::X) -> Self {
        Self(x)
    }
}
impl Accumulates for Reduces<bool> {
    type X = bool;

    fn update(&mut self, other: &Self::X) {
        self.0 |= *other
    }

    fn neutral() -> Self
    where
        Self::X: HasNeutral,
    {
        Self(Self::X::neutral())
    }

    fn from_iter<'a, I: IntoIterator<Item = &'a Self::X>>(iter: I) -> Self
    where
        Self: 'a,
    {
        Self(*iter.into_iter().min().unwrap())
    }

    fn result(&self) -> &Self::X {
        &self.0
    }

    fn from(x: Self::X) -> Self {
        Self(x)
    }
}
impl Accumulates for Reduces<()> {
    type X = ();

    fn update(&mut self, other: &Self::X) {}

    fn neutral() -> Self
    where
        Self::X: HasNeutral,
    {
        Self(())
    }

    fn from_iter<'a, I: IntoIterator<Item = &'a Self::X>>(iter: I) -> Self
    where
        Self: 'a,
    {
        Self(())
    }

    fn result(&self) -> &Self::X {
        &()
    }

    fn from(x: Self::X) -> Self {
        Self(x)
    }
}
impl Accumulates for Reduces<Void> {
    type X = Void;

    fn update(&mut self, other: &Self::X) {}

    fn neutral() -> Self
    where
        Self::X: HasNeutral,
    {
        Self(Void)
    }

    fn from_iter<'a, I: IntoIterator<Item = &'a Self::X>>(iter: I) -> Self
    where
        Self: 'a,
    {
        Self(Void)
    }

    fn result(&self) -> &Self::X {
        &Void
    }

    fn from(x: Self::X) -> Self {
        Self(x)
    }
}

/// Encapsulates a run piece.
#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct RunSignature<Idx, Q, C>(Idx, Q, C);

impl<Idx: IndexType, Q: Accumulates, C: Accumulates> RunSignature<Idx, Q, C> {
    pub fn state(&self) -> Idx {
        self.0
    }

    pub fn sc(&self) -> &Q::X {
        self.1.result()
    }

    pub fn ec(&self) -> &C::X {
        self.2.result()
    }
}

impl<Idx: IndexType, Q: Accumulates, C: Accumulates> RunSignature<Idx, Q, C> {
    pub fn empty_from(q: Idx) -> Self {
        Self(q, Q::neutral(), C::neutral())
    }
    fn dup(&self) -> Self {
        Self(self.0, self.1.clone(), self.2.clone())
    }

    fn with_q(self, q: Idx) -> Self {
        Self(q, self.1, self.2)
    }

    fn with_sc(self, sc: Q::X) -> Self {
        Self(self.0, Q::from(sc), self.2)
    }

    fn with_ec(self, ec: C::X) -> Self {
        Self(self.0, self.1, C::from(ec))
    }
    pub fn extend_in<Ts>(&self, ts: &Ts, symbol: SymbolOf<Ts>) -> Option<Self>
    where
        Ts: Deterministic<StateIndex = Idx, StateColor = Q::X, EdgeColor = C::X>,
    {
        match ts.transition(self.0, symbol) {
            Some(tt) => {
                let target = tt.target();
                let mut sc = Q::from_or_neutral(ts.state_color(target));
                sc.update(self.sc());
                let mut ec = C::from(tt.color().clone());
                ec.update(self.ec());

                Some(Self(target, sc, ec))
            }
            None => None,
        }
    }
    pub fn all_extensions<
        'a,
        Ts: Deterministic<StateIndex = Idx, StateColor = Q::X, EdgeColor = C::X>,
    >(
        &'a self,
        ts: &'a Ts,
    ) -> impl Iterator<Item = (SymbolOf<Ts>, Self)> + 'a {
        ts.alphabet()
            .universe()
            .filter_map(|sym| self.extend_in(ts, sym).map(|x| (sym, x)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RunProfile<Idx, Q, C>(Vec<RunSignature<Idx, Q, C>>);

impl<Idx, Q, C> RunProfile<Idx, Q, C> {
    pub fn iter(&self) -> impl Iterator<Item = &'_ RunSignature<Idx, Q, C>> + '_ {
        self.0.iter()
    }
}

impl<Idx: IndexType, Q: Accumulates, C: Accumulates> Show for RunProfile<Idx, Q, C>
where
    Q::X: Debug,
    C::X: Debug,
{
    fn show(&self) -> String {
        let mut out = String::new();
        for (i, rs) in self.0.iter().enumerate() {
            out.push_str(&format!(
                "{i} -{:?}|{:?}-> {}",
                rs.sc(),
                rs.ec(),
                rs.state()
            ))
        }
        out
    }

    fn show_collection<'a, I: IntoIterator<Item = &'a Self>>(iter: I) -> String
    where
        Self: 'a,
    {
        todo!()
    }
}

impl<Idx: IndexType, Q: Accumulates, C: Accumulates> RunProfile<Idx, Q, C> {
    pub fn empty<Ts: TransitionSystem<StateIndex = Idx, StateColor = Q::X, EdgeColor = C::X>>(
        ts: Ts,
    ) -> Self {
        Self::empty_for_states(ts.state_indices().collect(), ts)
    }

    pub fn empty_for_states<
        Ts: TransitionSystem<StateIndex = Idx, StateColor = Q::X, EdgeColor = C::X>,
    >(
        states: Vec<Idx>,
        ts: Ts,
    ) -> Self {
        Self(
            states
                .into_iter()
                .sorted()
                .map(|q| RunSignature::empty_from(q))
                .collect(),
        )
    }

    pub fn extend_in<Ts>(&self, ts: &Ts, sym: SymbolOf<Ts>) -> Self
    where
        Ts: Deterministic<StateIndex = Idx, StateColor = Q::X, EdgeColor = C::X>,
    {
        Self(
            self.0
                .iter()
                .map(|tp| {
                    tp.extend_in(&ts, sym)
                        .expect("we assume the ts to be complete")
                })
                .collect(),
        )
    }

    pub fn all_extensions<
        'a,
        Ts: Deterministic<StateIndex = Idx, StateColor = Q::X, EdgeColor = C::X>,
    >(
        &'a self,
        ts: &'a Ts,
    ) -> impl Iterator<Item = (Self, SymbolOf<Ts>)> + 'a {
        ts.alphabet()
            .universe()
            .map(|sym| (self.extend_in(ts, sym), sym))
    }
}

#[derive(Debug, Clone)]
enum ProfileEntry {
    Profile(usize),
    Redirect(usize),
}

impl From<ProfileEntry> for usize {
    fn from(value: ProfileEntry) -> Self {
        match value {
            ProfileEntry::Profile(p) => p,
            ProfileEntry::Redirect(r) => r,
        }
    }
}
impl From<&ProfileEntry> for usize {
    fn from(value: &ProfileEntry) -> Self {
        match value {
            ProfileEntry::Profile(p) => *p,
            ProfileEntry::Redirect(r) => *r,
        }
    }
}

#[derive(Clone)]
pub struct TransitionMonoid<
    'a,
    Ts: TransitionSystem,
    SA: Accumulates<X = Ts::StateColor>,
    EA: Accumulates<X = Ts::EdgeColor>,
> {
    ts: &'a Ts,
    strings: Vec<(Vec<SymbolOf<Ts>>, ProfileEntry)>,
    #[allow(clippy::type_complexity)]
    tps: Vec<(RunProfile<Ts::StateIndex, SA, EA>, usize)>,
}

impl<'a, Ts> TransitionMonoid<'a, Ts, Reduces<Ts::StateColor>, Reduces<Ts::EdgeColor>>
where
    Ts: Deterministic + Pointed,
    Reduces<Ts::EdgeColor>: Accumulates<X = Ts::EdgeColor>,
    Reduces<Ts::StateColor>: Accumulates<X = Ts::StateColor>,
{
    pub fn new_reducing(ts: &'a Ts) -> Self {
        Self::build(ts)
    }

    pub fn new_reducing_for_states<I: IntoIterator<Item = Ts::StateIndex>>(
        ts: &'a Ts,
        iter: I,
    ) -> Self {
        Self::build_for_states(ts, iter)
    }
}
impl<'a, Ts> TransitionMonoid<'a, Ts, Replaces<Ts::StateColor>, Replaces<Ts::EdgeColor>>
where
    Ts: Deterministic + Pointed,
    Replaces<Ts::EdgeColor>: Accumulates<X = Ts::EdgeColor>,
    Replaces<Ts::StateColor>: Accumulates<X = Ts::StateColor>,
{
    pub fn new_replacing(ts: &'a Ts) -> Self {
        Self::build(ts)
    }

    pub fn new_replacing_for_states<I: IntoIterator<Item = Ts::StateIndex>>(
        ts: &'a Ts,
        iter: I,
    ) -> Self {
        Self::build_for_states(ts, iter)
    }
}

impl<'a, Ts, SA: Accumulates<X = Ts::StateColor>, EA: Accumulates<X = Ts::EdgeColor>>
    TransitionMonoid<'a, Ts, SA, EA>
where
    Ts: Deterministic + Pointed,
{
    #[allow(clippy::type_complexity)]
    pub fn get_profile(
        &self,
        idx: usize,
    ) -> Option<(&RunProfile<Ts::StateIndex, SA, EA>, &[SymbolOf<Ts>])> {
        let (tp, string_idx) = self.tps.get(idx)?;
        let (word, _) = self.strings.get(*string_idx)?;
        Some((tp, word))
    }

    pub fn get_string(&self, idx: usize) -> Option<(&[SymbolOf<Ts>], usize)> {
        self.strings
            .get(idx)
            .map(|(word, profile)| (word.as_slice(), profile.into()))
    }

    pub fn profile_for<W: FiniteWord<SymbolOf<Ts>>>(&self, word: W) -> Option<usize> {
        let (tp, pe) = self.strings.iter().find(|(p, e)| p.equals(&word))?;
        match pe {
            ProfileEntry::Profile(p) => Some(*p),
            ProfileEntry::Redirect(r) => Some(*r),
        }
    }

    pub fn profile_indices(&self) -> std::ops::Range<usize> {
        0..self.tps.len()
    }

    fn build(ts: &'a Ts) -> TransitionMonoid<'a, Ts, SA, EA> {
        Self::build_for_states(ts, ts.state_indices())
    }

    fn build_for_states<I: IntoIterator<Item = Ts::StateIndex>>(
        ts: &'a Ts,
        iter: I,
    ) -> TransitionMonoid<'a, Ts, SA, EA> {
        let states = iter.into_iter().collect();
        let eps_profile = RunProfile::empty_for_states(states, ts);
        let mut tps = vec![(eps_profile.clone(), 0)];
        let mut strings = vec![(vec![], ProfileEntry::Profile(0))];
        let mut queue = VecDeque::from_iter([(vec![], eps_profile)]);

        while let Some((word, profile)) = queue.pop_front() {
            for (profile_extension, sym) in profile.all_extensions(ts) {
                assert!(
                    word.len() < 10,
                    "This looks dangerously like an infinite loop..."
                );
                let mut word_extension = word.clone();
                word_extension.push(sym);
                if let Some(existing_idx) = tps.iter().position(|(tp, _)| *tp == profile_extension)
                {
                    strings.push((word_extension, ProfileEntry::Redirect(existing_idx)));
                } else {
                    let string_id = strings.len();
                    let profile_id = tps.len();
                    strings.push((word_extension.clone(), ProfileEntry::Profile(profile_id)));
                    tps.push((profile_extension.clone(), string_id));
                    queue.push_back((word_extension, profile_extension));
                    assert!(profile_id < tps.len());
                    assert!(string_id < strings.len());
                }
            }
        }

        TransitionMonoid { ts, tps, strings }
    }
}

pub type ReplacingMonoid<'a, Ts> = TransitionMonoid<
    'a,
    Ts,
    Replaces<<Ts as TransitionSystem>::StateColor>,
    Replaces<<Ts as TransitionSystem>::EdgeColor>,
>;

pub type ReducingMonoid<'a, Ts> = TransitionMonoid<
    'a,
    Ts,
    Reduces<<Ts as TransitionSystem>::StateColor>,
    Reduces<<Ts as TransitionSystem>::EdgeColor>,
>;

impl<'a, Ts, SA, EA> Display for TransitionMonoid<'a, Ts, SA, EA>
where
    Ts: TransitionSystem,
    Ts::StateIndex: Show,
    for<'b> (&'b StateColor<Ts>, &'b EdgeColor<Ts>): Show,
    SA: Accumulates<X = Ts::StateColor>,
    EA: Accumulates<X = Ts::EdgeColor>,
    SymbolOf<Ts>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use owo_colors::OwoColorize;
        let mut b = tabled::builder::Builder::default();

        for (word, entry) in &self.strings {
            let mut row = vec![word.show()];
            let profile_idx = match entry {
                ProfileEntry::Profile(profile) => profile,
                ProfileEntry::Redirect(redirect) => redirect,
            };

            let (profile, _) = self.tps.get(*profile_idx).expect("Must exist!");
            row.extend(profile.iter().map(|tp| {
                format!(
                    "{}|{}",
                    tp.state().blue(),
                    (tp.sc(), tp.ec()).show().purple(),
                )
            }));
            b.push_record(row);
        }

        write!(f, "{}", b.build().with(tabled::settings::Style::ascii()))
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use crate::{congruence::transitionprofile::ReducingMonoid, tests::wiki_dfa, TransitionSystem};

    use super::TransitionMonoid;

    #[test]
    fn tp_from_ts() {
        let dfa = wiki_dfa();

        let tps = ReducingMonoid::build(&dfa);
        println!("{}", tps);

        let reduced_tps =
            ReducingMonoid::build_for_states(&dfa, dfa.state_indices().filter(|i| i % 2 == 0));
        print!("{}", reduced_tps);
    }
}
