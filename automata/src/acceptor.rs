use crate::{
    acceptance::AcceptanceCondition,
    run::Run,
    ts::{Pointed, TransitionSystem},
    words::Word,
};

/// Implemented by objects which can accept a word. We use `W` as an ipnut type parameter to allow for different implementations based on the type of word.
pub trait Acceptor<'ts, 'w, W: 'w> {
    /// The transition system type.
    type TS: TransitionSystem + Pointed + AcceptanceCondition;

    /// Returns true iff the given `word` is accepted, i.e. it satisfies the acceptance condition.
    fn accepts(&'ts self, word: &W) -> bool;
}

impl<'ts, 'w, TS, W> Acceptor<'ts, 'w, W> for TS
where
    W: Word + 'w,
    TS: TransitionSystem<S = W::S>
        + Pointed
        + Run<W, W::Kind>
        + AcceptanceCondition<Kind = W::Kind, Induced = <TS as Run<W, W::Kind>>::Induces>,
{
    type TS = TS;

    fn accepts(&'ts self, word: &W) -> bool {
        match self.run(self.initial(), word) {
            Ok(induced) => self.is_accepting(&induced),
            Err(_) => false,
        }
    }
}

#[cfg(test)]
mod tests {}
