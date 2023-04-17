use crate::{
    acceptance::AcceptanceCondition,
    run::{Configuration, Evaluate},
    ts::{Pointed, TransitionSystem},
    Word,
};

/// Implemented by objects which can accept a word. We use `W` as an ipnut type parameter to allow for different implementations based on the type of word.
pub trait Acceptor {
    /// The transition system type.
    type TS: TransitionSystem + Pointed;

    /// The acceptance condition type.
    type Acc: AcceptanceCondition;

    /// Returns true iff the given `word` is accepted, i.e. it satisfies the acceptance condition.
    fn accepts<'t, W>(&'t self, on: W) -> bool
    where
        Configuration<&'t Self::TS, W>:
            Evaluate<Output = <Self::Acc as AcceptanceCondition>::Induced>,
        W: Word<S = <Self::TS as TransitionSystem>::Input>;

    /// Returns the opposite of `accepts`.
    fn rejects<'t, W>(&'t self, on: W) -> bool
    where
        Configuration<&'t Self::TS, W>:
            Evaluate<Output = <Self::Acc as AcceptanceCondition>::Induced>,
        W: Word<S = <Self::TS as TransitionSystem>::Input>,
    {
        !self.accepts(on)
    }
}

impl<TS, Acc> Acceptor for (TS, Acc)
where
    TS: TransitionSystem + Pointed,
    Acc: AcceptanceCondition,
{
    type TS = TS;

    type Acc = Acc;

    fn accepts<'t, W>(&'t self, on: W) -> bool
    where
        Configuration<&'t Self::TS, W>:
            Evaluate<Output = <Self::Acc as AcceptanceCondition>::Induced>,
        W: Word<S = <Self::TS as TransitionSystem>::Input>,
    {
        matches!(self.0.run_word_from(on, self.0.initial()).evaluate(), Ok(_))
    }
}

impl<A> Acceptor for A
where
    A: TransitionSystem + Pointed + AcceptanceCondition,
{
    type TS = A;
    type Acc = A;

    fn accepts<'t, W>(&'t self, on: W) -> bool
    where
        Configuration<&'t Self::TS, W>:
            Evaluate<Output = <Self::Acc as AcceptanceCondition>::Induced>,
        W: Word<S = <Self::TS as TransitionSystem>::Input>,
    {
        match self.run_word_from(on, self.initial()).evaluate() {
            Ok(induced) => self.is_accepting(&induced),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {}
