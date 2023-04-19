use crate::{ts::TriggerOf, Combined, Pointed, TransitionSystem};
use std::{borrow::Borrow, hash::Hash};

use super::{MutableMapping, Priority};

pub struct TransducerInputIterator<'me, M: Transducer, I> {
    machine: &'me M,
    state: M::State,
    input: I,
}

impl<'me, M: Transducer, B: Borrow<M::Input>, I> Iterator for TransducerInputIterator<'me, M, I>
where
    I: Iterator<Item = B>,
{
    type Item = (M::State, M::Output);

    fn next(&mut self) -> Option<Self::Item> {
        self.input.next().map(|s| {
            let next = self.machine.succ(&self.state, s.borrow()).unwrap();
            let output = self
                .machine
                .trigger_output(&(self.state.clone(), s.borrow().clone()));
            self.state = next;
            (self.state.clone(), output)
        })
    }
}

/// A Mealy machine is a transition system with output.
pub trait Transducer: TransitionSystem {
    /// The output type.
    type Output: Eq + Hash;

    /// Returns the output of the transition from `from` on `on`.
    fn trigger_output(&self, from: &TriggerOf<Self>) -> Self::Output;

    /// Sets the output of the transition from `from` on `on`. Returns the previous output, if any.
    fn set_output(&mut self, from: &TriggerOf<Self>, output: Self::Output) -> Option<Self::Output>;

    /// Computes the output on the given word starting in the initial state.
    fn output_iter<B: Borrow<Self::Input>, W: IntoIterator<Item = B>>(
        &self,
        word: W,
    ) -> TransducerInputIterator<'_, Self, W::IntoIter>
    where
        Self: Sized + Pointed,
    {
        self.output_iter_from(&self.initial(), word)
    }

    /// Computes the _last_ output produced by the given word starting in the initial state.
    fn output<B: Borrow<Self::Input>, W: IntoIterator<Item = B>>(
        &self,
        word: W,
    ) -> Option<Self::Output>
    where
        Self: Sized + Pointed,
    {
        self.output_iter(word).map(|(_, o)| o).last()
    }

    /// Returns the output running all symbols in `word` from `from`.
    fn output_iter_from<B: Borrow<Self::Input>, W: IntoIterator<Item = B>>(
        &self,
        from: &Self::State,
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

impl<TS, P> Transducer for Combined<TS, P>
where
    TS: TransitionSystem,
    P: MutableMapping<Domain = TriggerOf<TS>>,
{
    type Output = P::Range;

    fn trigger_output(&self, from: &TriggerOf<Self>) -> Self::Output {
        self.acceptance().get_value(from)
    }

    fn set_output(&mut self, from: &TriggerOf<Self>, output: Self::Output) -> Option<Self::Output> {
        self.acceptance_mut().set_value(from, output)
    }
}
