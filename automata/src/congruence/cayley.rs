use std::marker::PhantomData;

use super::{
    transitionprofile::{Reduces, Replaces},
    Accumulates, RunProfile, TransitionMonoid,
};
use crate::{
    alphabet::{Directional, InvertibleChar},
    prelude::*,
    Map,
};

#[derive(Clone)]
pub struct Cayley<
    'a,
    Ts: TransitionSystem<Alphabet = Simple> + Pointed,
    SA: Accumulates<X = Ts::StateColor>,
    EA: Accumulates<X = Ts::EdgeColor>,
> {
    ts: &'a Ts,
    alphabet: Directional,
    m: TransitionMonoid<'a, Ts, SA, EA>,
}

impl<
        'a,
        Ts: TransitionSystem<Alphabet = Simple> + Pointed,
        SA: Accumulates<X = Ts::StateColor>,
        EA: Accumulates<X = Ts::EdgeColor>,
    > Cayley<'a, Ts, SA, EA>
{
    pub fn ts(&self) -> &Ts {
        &self.ts
    }

    pub fn monoid(&self) -> &TransitionMonoid<'a, Ts, SA, EA> {
        &self.m
    }
}

impl<'a, Ts, SA: Accumulates<X = Ts::StateColor>, EA: Accumulates<X = Ts::EdgeColor>>
    TransitionSystem for Cayley<'a, Ts, SA, EA>
where
    Ts: TransitionSystem<Alphabet = Simple> + Pointed,
{
    type StateIndex = usize;

    type StateColor = RunProfile<Ts::StateIndex, SA, EA>;

    type EdgeColor = ();

    type TransitionRef<'this> = (InvertibleChar, usize, ()) where Self: 'this;

    type StateIndices<'this> = std::ops::Range<usize> where Self: 'this;

    type EdgesFromIter<'this> = GenericEdgesFrom<'this, Self> where Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.monoid().profile_indices()
    }

    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        let idx = state.to_index(self)?;
        let (tp, string) = self.monoid().get_profile(idx)?;
        let mut word = string.to_deque();
        symbol.mul(&mut word);
        let tp = self.monoid().profile_for(&word)?;
        Some((symbol, tp, ()))
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        Some(GenericEdgesFrom::new(self, state.to_index(self)?))
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        self.monoid().get_profile(state).map(|p| p.0.clone())
    }
}

impl<
        'a,
        Ts: TransitionSystem<Alphabet = Simple> + Pointed,
        SA: Accumulates<X = Ts::StateColor>,
        EA: Accumulates<X = Ts::EdgeColor>,
    > HasAlphabet for Cayley<'a, Ts, SA, EA>
{
    type Alphabet = Directional;
    fn alphabet(&self) -> &Self::Alphabet {
        &self.alphabet
    }
}

impl<'a, Ts> Cayley<'a, Ts, Reduces<Ts::StateColor>, Reduces<Ts::EdgeColor>>
where
    Ts: TransitionSystem<Alphabet = Simple> + Pointed,
    Reduces<Ts::EdgeColor>: Accumulates<X = Ts::EdgeColor>,
    Reduces<Ts::StateColor>: Accumulates<X = Ts::StateColor>,
{
    pub fn new_reducing(ts: &'a Ts) -> Self {
        Cayley {
            ts,
            alphabet: Directional::from_alphabet(ts.alphabet()),
            m: TransitionMonoid::new_reducing(ts),
        }
    }
}
impl<'a, Ts> Cayley<'a, Ts, Replaces<Ts::StateColor>, Replaces<Ts::EdgeColor>>
where
    Ts: TransitionSystem<Alphabet = Simple> + Pointed,
    Replaces<Ts::EdgeColor>: Accumulates<X = Ts::EdgeColor>,
    Replaces<Ts::StateColor>: Accumulates<X = Ts::StateColor>,
{
    pub fn new_replacing(ts: &'a Ts) -> Self {
        Cayley {
            ts,
            alphabet: Directional::from_iter(ts.alphabet().universe().cloned()),
            m: TransitionMonoid::new_replacing(ts),
        }
    }
}

impl<'a, Ts, SA: Accumulates<X = Ts::StateColor>, EA: Accumulates<X = Ts::EdgeColor>>
    Cayley<'a, Ts, SA, EA>
where
    Ts: TransitionSystem<Alphabet = Simple> + Pointed,
    Ts::StateColor: Accumulates,
    Ts::EdgeColor: Accumulates,
{
    pub fn from(ts: &'a Ts, m: TransitionMonoid<'a, Ts, SA, EA>) -> Self {
        Self {
            ts,
            alphabet: Directional::from_iter(ts.alphabet().universe().cloned()),
            m,
        }
    }
}

// Now for a more specific, guaranteed working right cayley impl

#[derive(Clone)]
pub struct RightCayley<
    'a,
    Ts: TransitionSystem + Pointed,
    SA: Accumulates<X = Ts::StateColor>,
    EA: Accumulates<X = Ts::EdgeColor>,
> {
    ts: &'a Ts,
    m: TransitionMonoid<'a, Ts, SA, EA>,
}

impl<
        'a,
        Ts: TransitionSystem + Pointed,
        SA: Accumulates<X = Ts::StateColor>,
        EA: Accumulates<X = Ts::EdgeColor>,
    > RightCayley<'a, Ts, SA, EA>
{
    pub fn ts(&self) -> &Ts {
        &self.ts
    }

    pub fn monoid(&self) -> &TransitionMonoid<'a, Ts, SA, EA> {
        &self.m
    }
}

impl<'a, Ts, SA: Accumulates<X = Ts::StateColor>, EA: Accumulates<X = Ts::EdgeColor>>
    TransitionSystem for RightCayley<'a, Ts, SA, EA>
where
    Ts: TransitionSystem + Pointed,
{
    type StateIndex = usize;

    type StateColor = RunProfile<Ts::StateIndex, SA, EA>;

    type EdgeColor = ();

    type TransitionRef<'this> = (ExpressionOf<Ts>, usize, ()) where Self: 'this;

    type StateIndices<'this> = std::ops::Range<usize> where Self: 'this;

    type EdgesFromIter<'this> = GenericEdgesFrom<'this, Self> where Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        self.monoid().profile_indices()
    }

    fn transition<Idx: Indexes<Self>>(
        &self,
        state: Idx,
        symbol: SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        let idx = state.to_index(self)?;
        let (tp, string) = self.monoid().get_profile(idx)?;
        let mut word = string.to_vec();
        word.push(symbol);
        let tp = self.monoid().profile_for(&word)?;
        Some((<Ts::Alphabet as Alphabet>::expression(symbol), tp, ()))
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        Some(GenericEdgesFrom::new(self, state.to_index(self)?))
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        self.monoid().get_profile(state).map(|p| p.0.clone())
    }
}

impl<
        'a,
        Ts: TransitionSystem + Pointed,
        SA: Accumulates<X = Ts::StateColor>,
        EA: Accumulates<X = Ts::EdgeColor>,
    > HasAlphabet for RightCayley<'a, Ts, SA, EA>
{
    type Alphabet = Ts::Alphabet;
    fn alphabet(&self) -> &Self::Alphabet {
        self.ts().alphabet()
    }
}

impl<'a, Ts> RightCayley<'a, Ts, Reduces<Ts::StateColor>, Reduces<Ts::EdgeColor>>
where
    Ts: TransitionSystem + Pointed,
    Reduces<Ts::EdgeColor>: Accumulates<X = Ts::EdgeColor>,
    Reduces<Ts::StateColor>: Accumulates<X = Ts::StateColor>,
{
    pub fn new_reducing(ts: &'a Ts) -> Self {
        RightCayley {
            ts,
            m: TransitionMonoid::new_reducing(ts),
        }
    }
}
impl<'a, Ts> RightCayley<'a, Ts, Replaces<Ts::StateColor>, Replaces<Ts::EdgeColor>>
where
    Ts: TransitionSystem + Pointed,
    Replaces<Ts::EdgeColor>: Accumulates<X = Ts::EdgeColor>,
    Replaces<Ts::StateColor>: Accumulates<X = Ts::StateColor>,
{
    pub fn new_replacing(ts: &'a Ts) -> Self {
        RightCayley {
            ts,
            m: TransitionMonoid::new_replacing(ts),
        }
    }
}

impl<'a, Ts, SA: Accumulates<X = Ts::StateColor>, EA: Accumulates<X = Ts::EdgeColor>>
    RightCayley<'a, Ts, SA, EA>
where
    Ts: TransitionSystem + Pointed,
    Ts::StateColor: Accumulates,
    Ts::EdgeColor: Accumulates,
{
    pub fn from(ts: &'a Ts, m: TransitionMonoid<'a, Ts, SA, EA>) -> Self {
        Self { ts, m }
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::{tests::wiki_dfa, ts::ToDot};

    #[test]
    #[traced_test]
    fn right_cayley_graph() {
        let dfa = wiki_dfa();
        dfa.display_rendered();
        let accumulating_cayley = super::Cayley::new_reducing(&dfa);
        accumulating_cayley.display_rendered();
        let replacing_cayley = super::Cayley::new_replacing(&dfa);
        replacing_cayley.display_rendered();
    }
}
