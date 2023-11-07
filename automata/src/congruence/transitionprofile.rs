use std::{
    collections::{BTreeMap, VecDeque},
    fmt::{Debug, Display},
};

use itertools::Itertools;

use crate::{prelude::*, Map};

/// Encapsulates the behavour of colors along run segments. This is what we are mainly
/// interested in and which we want to keep track of. As an example consider a run segment, where
/// each edge outputs an `usize`. If we are intersted in semantics of a parity automaton,
/// then we mainly want to know what the combined behaviour of the visited edges is.
/// As we use min-parity, this means we take the minimal value along all edges.
pub trait Accumulates: Color + Sized {
    /// Updates self with the received value.
    fn update(&mut self, other: &Self);
    /// The neutral element with regard to accumulation, which is `false` for booleans and the maximal
    /// `usize::MAX` for integers.
    fn neutral() -> Self;
    /// Accumulate an iterator into a single value.
    fn from_iter<'a, I: IntoIterator<Item = &'a Self>>(iter: I) -> Self
    where
        Self: 'a,
    {
        iter.into_iter().fold(Self::neutral(), |mut acc, x| {
            acc.update(x);
            acc
        })
    }
}

impl Accumulates for usize {
    fn update(&mut self, other: &Self) {
        *self = std::cmp::min(*self, *other)
    }

    fn neutral() -> Self {
        usize::MAX
    }
}

impl Accumulates for bool {
    fn update(&mut self, other: &Self) {
        *self |= other
    }

    fn neutral() -> Self {
        false
    }
}

impl Accumulates for () {
    fn update(&mut self, other: &Self) {}

    fn neutral() -> Self {}
}

pub trait Accumulator<X> {
    type Result;

    fn process(&mut self, x: X);
    fn result(&self) -> Self::Result;
}

impl<X: Accumulates> Accumulator<X> for Vec<X> {
    type Result = X;

    fn process(&mut self, x: X) {
        self.push(x)
    }

    fn result(&self) -> Self::Result {
        X::from_iter(self.iter())
    }
}

impl<X: Accumulates + Clone> Accumulator<X> for X {
    type Result = X;

    fn process(&mut self, x: X) {
        self.update(&x)
    }

    fn result(&self) -> Self::Result {
        self.clone()
    }
}

/// Encapsulates a run piece.
#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct RunSignature<Idx, Q, C>(Idx, Q, C);

impl<Idx: IndexType, Q, C> RunSignature<Idx, Q, C> {
    pub fn state(&self) -> Idx {
        self.0
    }

    pub fn sc(&self) -> &Q {
        &self.1
    }

    pub fn ec(&self) -> &C {
        &self.2
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

    fn with_sc(self, sc: Q) -> Self {
        let mut sc = Q::neutral();
        sc.update(&self.1);
        Self(self.0, sc, self.2)
    }

    fn with_ec(self, ec: C) -> Self {
        let mut ec = C::neutral();
        ec.update(&self.2);
        Self(self.0, self.1, ec)
    }
    pub fn extend_in<Ts: TransitionSystem<StateIndex = Idx, StateColor = Q, EdgeColor = C>>(
        &self,
        ts: &Ts,
        symbol: SymbolOf<Ts>,
    ) -> Result<Self, ()> {
        match ts.transition(self.0, symbol) {
            Some(tt) => {
                let target = tt.target();
                let mut sc = ts.state_color(target).unwrap_or(Q::neutral());
                sc.update(self.sc());
                let mut ec = tt.color();
                ec.update(self.ec());

                Ok(Self(target, sc, ec))
            }
            None => Err(()),
        }
    }
    pub fn all_extensions<
        'a,
        Ts: TransitionSystem<StateIndex = Idx, StateColor = Q, EdgeColor = C>,
    >(
        &'a self,
        ts: &'a Ts,
    ) -> impl Iterator<Item = (SymbolOf<Ts>, Self)> + 'a {
        ts.alphabet()
            .universe()
            .filter_map(|sym| self.extend_in(ts, *sym).ok().map(|x| (*sym, x)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RunProfile<Idx, Q, C>(Vec<RunSignature<Idx, Q, C>>);

impl<Idx, Q, C> RunProfile<Idx, Q, C> {
    pub fn iter(&self) -> impl Iterator<Item = &'_ RunSignature<Idx, Q, C>> + '_ {
        self.0.iter()
    }
}

impl<Idx: IndexType, Q: Accumulates, C: Accumulates> RunProfile<Idx, Q, C> {
    pub fn empty<Ts: TransitionSystem<StateIndex = Idx, StateColor = Q, EdgeColor = C>>(
        ts: Ts,
    ) -> Self {
        Self(
            ts.state_indices()
                .map(|q| RunSignature::empty_from(q))
                .collect(),
        )
    }

    pub fn extend_in<Ts: TransitionSystem<StateIndex = Idx, StateColor = Q, EdgeColor = C>>(
        &self,
        ts: &Ts,
        sym: SymbolOf<Ts>,
    ) -> Self {
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
        Ts: TransitionSystem<StateIndex = Idx, StateColor = Q, EdgeColor = C>,
    >(
        &'a self,
        ts: &'a Ts,
    ) -> impl Iterator<Item = (Self, SymbolOf<Ts>)> + 'a {
        ts.alphabet()
            .universe()
            .map(|sym| (self.extend_in(ts, *sym), *sym))
    }
}

#[derive(Debug, Clone)]
enum ProfileEntry {
    Profile(usize),
    Redirect(usize),
}

#[derive(Clone, Debug)]
pub struct TransitionProfiles<'a, Ts: TransitionSystem> {
    ts: &'a Ts,
    strings: Vec<(Vec<SymbolOf<Ts>>, ProfileEntry)>,
    #[allow(clippy::type_complexity)]
    tps: Vec<(
        RunProfile<Ts::StateIndex, Ts::StateColor, Ts::EdgeColor>,
        usize,
    )>,
}

impl<'a, Ts> TransitionProfiles<'a, Ts>
where
    Ts: TransitionSystem + Pointed,
    Ts::StateColor: Accumulates,
    Ts::EdgeColor: Accumulates,
{
    pub fn new(ts: &'a Ts) -> Self {
        let mut tps = vec![(RunProfile::empty(&ts), 0)];
        let mut strings = vec![(vec![], ProfileEntry::Profile(0))];
        let mut queue = VecDeque::from_iter([(vec![], RunProfile::empty(&ts))]);

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

        Self { ts, tps, strings }
    }
}

trait Show {
    fn show(&self) -> String;
}

impl Show for usize {
    fn show(&self) -> String {
        self.to_string()
    }
}

impl Show for () {
    fn show(&self) -> String {
        "-".into()
    }
}

impl Show for bool {
    fn show(&self) -> String {
        match self {
            true => "+",
            false => "-",
        }
        .to_string()
    }
}

impl<'a, Ts> Display for TransitionProfiles<'a, Ts>
where
    Ts: TransitionSystem,
    Ts::StateIndex: Show,
    Ts::EdgeColor: Show,
    Ts::StateColor: Show,
    SymbolOf<Ts>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use owo_colors::OwoColorize;
        let mut b = tabled::builder::Builder::default();
        b.set_header(
            std::iter::once("word".to_string())
                .chain(self.ts.state_indices().map(|id| id.to_string())),
        );
        println!("{:?}", self.tps.len());
        for (word, entry) in &self.strings {
            let mut row = vec![word.display()];
            let profile_idx = match entry {
                ProfileEntry::Profile(profile) => profile,
                ProfileEntry::Redirect(redirect) => redirect,
            };

            println!(
                "Getting ID {profile_idx} for {:?} of word {:?}",
                entry, word
            );
            let (profile, _) = self.tps.get(*profile_idx).expect("Must exist!");
            row.extend(profile.iter().map(|tp| {
                format!(
                    "{}|{}|{}",
                    tp.state().blue(),
                    tp.sc().show().purple(),
                    tp.ec().show().green()
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

    use crate::tests::wiki_dfa;

    use super::TransitionProfiles;

    #[test]
    fn tp_from_ts() {
        let dfa = wiki_dfa();

        let tps = TransitionProfiles::new(&dfa);
        println!("{:#?}", tps);
        println!("{}", tps)
    }
}
