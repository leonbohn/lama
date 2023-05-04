use crate::{
    acceptance::AcceptanceCondition,
    run::{Configuration, Evaluate},
    ts::{InputOf, Pointed, Successor},
    Word,
};

/// Implemented by objects which can accept a word. We use `W` as an ipnut type parameter to allow for different implementations based on the type of word.
pub trait Acceptor {
    /// The transition system type.
    type TS: Successor + Pointed;

    /// The acceptance condition type.
    type Acc: AcceptanceCondition;

    /// Returns true iff the given `word` is accepted, i.e. it satisfies the acceptance condition.
    fn accepts<'t, W>(&'t self, on: W) -> bool
    where
        Configuration<&'t Self::TS, W>:
            Evaluate<Output = <Self::Acc as AcceptanceCondition>::Induced>,
        W: Word<S = InputOf<Self::TS>>;

    /// Returns the opposite of `accepts`.
    fn rejects<'t, W>(&'t self, on: W) -> bool
    where
        Configuration<&'t Self::TS, W>:
            Evaluate<Output = <Self::Acc as AcceptanceCondition>::Induced>,
        W: Word<S = InputOf<Self::TS>>,
    {
        !self.accepts(on)
    }
}

impl<TS, Acc> Acceptor for (TS, Acc)
where
    TS: Successor + Pointed,
    Acc: AcceptanceCondition,
{
    type TS = TS;

    type Acc = Acc;

    fn accepts<'t, W>(&'t self, on: W) -> bool
    where
        Configuration<&'t Self::TS, W>:
            Evaluate<Output = <Self::Acc as AcceptanceCondition>::Induced>,
        W: Word<S = InputOf<Self::TS>>,
    {
        matches!(self.0.run_word_from(on, self.0.initial()).evaluate(), Ok(_))
    }
}

impl<A> Acceptor for A
where
    A: Successor + Pointed + AcceptanceCondition,
{
    type TS = A;
    type Acc = A;

    fn accepts<'t, W>(&'t self, on: W) -> bool
    where
        Configuration<&'t Self::TS, W>:
            Evaluate<Output = <Self::Acc as AcceptanceCondition>::Induced>,
        W: Word<S = InputOf<Self::TS>>,
    {
        match self.run_word_from(on, self.initial()).evaluate() {
            Ok(induced) => self.is_accepting(&induced),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {}
