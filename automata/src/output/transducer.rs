use crate::{ts::TriggerOf, Combined, TransitionSystem};
use std::hash::Hash;

use super::{MutableMapping, Priority};

pub struct TransducerInputIterator<'me, M: Transducer, I> {
    machine: &'me M,
    state: M::State,
    input: I,
}

impl<'me, M: Transducer, I> Iterator for TransducerInputIterator<'me, M, I>
where
    I: Iterator<Item = M::Input>,
{
    type Item = (M::State, M::Output);

    fn next(&mut self) -> Option<Self::Item> {
        self.input.next().map(|s| {
            let next = self.machine.succ(&self.state, &s).unwrap();
            let output = self.machine.output(&(self.state.clone(), s));
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
    fn output(&self, from: &TriggerOf<Self>) -> Self::Output;

    /// Sets the output of the transition from `from` on `on`. Returns the previous output, if any.
    fn set_output(&mut self, from: &TriggerOf<Self>, output: Self::Output) -> Option<Self::Output>;

    /// Returns the output running all symbols in `word` from `from`.
    fn output_word<W: IntoIterator<Item = Self::Input>>(
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
    type Output = Priority;

    fn output(&self, from: &TriggerOf<Self>) -> Self::Output {
        self.acceptance().priority(from)
    }

    fn set_output(&mut self, from: &TriggerOf<Self>, output: Self::Output) -> Option<Self::Output> {
        self.acceptance_mut().set_priority(from, output)
    }
}
