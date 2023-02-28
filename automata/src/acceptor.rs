use crate::{
    acceptance::AcceptanceCondition,
    run::Run,
    ts::{Pointed, SymbolOf, TransitionSystem},
    words::Word,
    Boundedness,
};

/// Implemented by objects which can accept a word. We use `W` as an ipnut type parameter to allow for different implementations based on the type of word.
pub trait Acceptor<K> {
    /// The transition system type.
    type TS: TransitionSystem + Pointed + AcceptanceCondition;

    /// Returns true iff the given `word` is accepted, i.e. it satisfies the acceptance condition.
    fn accepts<W: Run<Self::TS, K, Induces = <Self::TS as AcceptanceCondition>::Induced>>(
        &self,
        word: &W,
    ) -> bool;
}

impl<TS, K> Acceptor<K> for TS
where
    K: Boundedness,
    TS: TransitionSystem + Pointed + AcceptanceCondition,
{
    type TS = TS;

    fn accepts<W: Run<Self::TS, K, Induces = <Self::TS as AcceptanceCondition>::Induced>>(
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
