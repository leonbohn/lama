use crate::{
    acceptance::AcceptanceCondition,
    run::Run,
    ts::{Pointed, TransitionSystem},
    Boundedness,
};

/// Implemented by objects which can accept a word. We use `W` as an ipnut type parameter to allow for different implementations based on the type of word.
pub trait Acceptor<K> {
    /// The transition system type.
    type TS: TransitionSystem + Pointed;

    /// The acceptance condition type.
    type Acc: AcceptanceCondition<Kind = K>;

    /// Returns true iff the given `word` is accepted, i.e. it satisfies the acceptance condition.
    fn accepts<W: Run<Self::TS, K, Induces = <Self::Acc as AcceptanceCondition>::Induced>>(
        &self,
        word: &W,
    ) -> bool;

    /// Returns the opposite of `accepts`.
    fn rejects<W: Run<Self::TS, K, Induces = <Self::Acc as AcceptanceCondition>::Induced>>(
        &self,
        word: &W,
    ) -> bool {
        !self.accepts(word)
    }
}

impl<TS, K> Acceptor<K> for TS
where
    K: Boundedness,
    TS: TransitionSystem + Pointed + AcceptanceCondition<Kind = K>,
{
    type TS = TS;

    type Acc = TS;

    fn accepts<W: Run<Self::TS, K, Induces = <Self::Acc as AcceptanceCondition>::Induced>>(
        &self,
        word: &W,
    ) -> bool {
        match word.run(self, self.initial()) {
            Ok(induced) => self.is_accepting(&induced),
            Err(_) => false,
        }
    }
}

#[cfg(test)]
mod tests {}
