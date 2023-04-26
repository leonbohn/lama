use crate::{
    output::{IntoAssigments, Mapping},
    Combined, StateIndex, Symbol, TransitionSystem, Value,
};

impl<Q: StateIndex, S: Symbol, X: Value> Combined<TransitionSystem<Q, S>, Mapping<Q, X>> {
    pub fn map_acceptance<Y, F>(self, f: F) -> Combined<TransitionSystem<Q, S>, Mapping<Q, Y>>
    where
        Y: Value,
        F: Fn(&X) -> Y + Copy,
    {
        let acc = self
            .acceptance()
            .into_assignments()
            .map(|x| x.map_right(f))
            .collect();
        Combined::from_parts(self.ts, self.initial, acc)
    }
}
