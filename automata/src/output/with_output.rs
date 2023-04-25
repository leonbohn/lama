use impl_tools::autoimpl;
use itertools::Itertools;

use crate::{ts::TriggerOf, Combined, Pointed, Successor, Symbol, Transformer};
use std::borrow::Borrow;

#[autoimpl(for<T: trait> &T, &mut T)]
pub trait HasOutput {
    type Gamma: Symbol;
    type Output<'me>: Iterator<Item = &'me Self::Gamma>
    where
        Self: 'me;

    fn output_alphabet_iter(&self) -> Self::Output<'_>;

    fn output_alphabet(&self) -> itertools::Unique<Self::Output<'_>> {
        self.output_alphabet_iter().unique()
    }
}

/// A Mealy machine is a transition system with output.
pub trait TransitionOutput: Successor + HasOutput {
    /// Returns the output of the transition from `from` on `on`.
    fn trigger_output(&self, from: &TriggerOf<Self>) -> Self::Gamma;

    /// Computes the output on the given word starting in the initial state.
    fn output_iter<B: Borrow<Self::Sigma>, W: IntoIterator<Item = B>>(
        &self,
        word: W,
    ) -> TransducerInputIterator<'_, Self, W::IntoIter>
    where
        Self: Sized + Pointed,
    {
        self.output_iter_from(&self.initial(), word)
    }

    /// Computes the _last_ output produced by the given word starting in the initial state.
    fn output<B: Borrow<Self::Sigma>, W: IntoIterator<Item = B>>(
        &self,
        word: W,
    ) -> Option<Self::Gamma>
    where
        Self: Sized + Pointed,
    {
        self.output_iter(word).map(|(_, o)| o).last()
    }

    /// Returns the output running all symbols in `word` from `from`.
    fn output_iter_from<B: Borrow<Self::Sigma>, W: IntoIterator<Item = B>>(
        &self,
        from: &Self::Q,
        word: W,
    ) -> TransducerInputIterator<'_, Self, W::IntoIter>
    where
        Self: Sized,
    {
        TransducerInputIterator {
            machine: self,
            state: from.clone(),
            input: word.into_iter(),
        }
    }
}

pub struct TransducerInputIterator<'me, M: TransitionOutput, I> {
    machine: &'me M,
    state: M::Q,
    input: I,
}

impl<'me, M: TransitionOutput, B: Borrow<M::Sigma>, I> Iterator
    for TransducerInputIterator<'me, M, I>
where
    I: Iterator<Item = B>,
{
    type Item = (M::Q, M::Gamma);

    fn next(&mut self) -> Option<Self::Item> {
        self.input.next().map(|s| {
            let next = self.machine.successor(&self.state, s.borrow()).unwrap();
            let output = self
                .machine
                .trigger_output(&(self.state.clone(), s.borrow().clone()));
            self.state = next;
            (self.state.clone(), output)
        })
    }
}

pub trait ModifyOutput: TransitionOutput {
    /// Sets the output of the transition from `from` on `on`. Returns the previous output, if any.
    fn set_output(&mut self, from: &TriggerOf<Self>, output: Self::Gamma) -> Option<Self::Gamma>;
}

impl<TS, P> HasOutput for Combined<TS, P>
where
    TS: Successor,
    P: HasOutput,
{
    type Gamma = P::Gamma;

    type Output<'me> = P::Output<'me>
    where
        Self: 'me;

    fn output_alphabet_iter(&self) -> Self::Output<'_> {
        self.acceptance().output_alphabet_iter()
    }
}

impl<TS, P> TransitionOutput for Combined<TS, P>
where
    TS: Successor,
    P: HasOutput + Transformer<Domain = TriggerOf<TS>, Range = P::Gamma>,
{
    fn trigger_output(&self, from: &TriggerOf<Self>) -> Self::Gamma {
        self.acceptance().apply(from)
    }
}
