use crate::{
    acceptance::AcceptanceCondition,
    run::{RunResult, Walk},
    ts::{Pointed, TransitionSystem},
    words::Word,
};

/// Implemented by objects which can accept a word. We use `W` as an ipnut type parameter to allow for different implementations based on the type of word.
pub trait Acceptor<'ts, 'w, W: Word + 'w> {
    /// The transition system type.
    type TS: TransitionSystem;

    /// Returns true iff the given `word` is accepted, i.e. it satisfies the acceptance condition.
    fn accepts<I>(&'ts self, word: I) -> bool
    where
        &'w W: From<I>;
}

impl<'ts, 'w, W, TS> Acceptor<'ts, 'w, W> for TS
where
    W: Word + 'w,
    TS: TransitionSystem + Pointed + AcceptanceCondition + Walk<'ts, 'w, W> + 'ts,
    TS::Walker: RunResult<W, Success = <TS as AcceptanceCondition>::Induced>,
{
    type TS = TS;

    fn accepts<I>(&'ts self, word: I) -> bool
    where
        &'w W: From<I>,
    {
        let mut walker = self.walk_from_on(self.initial(), word.into());
        match walker.result() {
            Ok(induced) => self.is_accepting(&induced),
            Err(_) => false,
        }
    }
}
