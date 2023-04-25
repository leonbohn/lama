use std::sync::Arc;

use crate::{
    output::{Assignment, AssignmentReference, IntoAssigments, Mapping},
    ts::{
        transitionsystem::{States, Transitions},
        IntoTransitions, TransitionReference, TriggerOf,
    },
    Combined, MealyMachine, Pair, Pointed, StateIndex, Symbol, Transition, TransitionSystem,
    Trigger, Value,
};

pub type AssignmentProductMapping<X, Y, A> =
    Mapping<X, Pair<Y, <<A as IntoAssigments>::AssignmentRef as Assignment>::Right>>;

impl<X, Y> Mapping<X, Y>
where
    X: Value,
    Y: Value,
{
    /// Builds the product mapping with the given one.
    pub fn product_with_assignments<I, A, B>(
        &self,
        assignments: I,
    ) -> Mapping<Pair<X, A>, Pair<Y, B>>
    where
        I: IntoAssigments,
        I::AssignmentRef: Assignment<Left = A, Right = B>,
        A: Value,
        B: Value,
    {
        self.into_assignments()
            .flat_map(|a| {
                assignments.into_assignments().into_iter().map(move |b| {
                    (
                        Pair::new(a.left(), b.left()),
                        Pair::new(a.right(), b.right()),
                    )
                })
            })
            .collect()
    }
}

impl<Q: StateIndex, S: Symbol> TransitionSystem<Q, S> {
    /// Builds the product transition system with the given one.
    pub fn product_with_transitions<T: IntoTransitions<Sigma = S>>(
        &self,
        transitions: T,
    ) -> TransitionSystem<Pair<Q, T::Q>, S> {
        self.into_transitions()
            .flat_map(|t| {
                transitions.into_transitions().filter_map(move |t2| {
                    if t.sym() == t2.sym() {
                        Some((
                            Pair {
                                left: t.source().clone(),
                                right: t2.source().clone(),
                            },
                            t.sym().clone(),
                            Pair {
                                left: t.target().clone(),
                                right: t2.target().clone(),
                            },
                        ))
                    } else {
                        None
                    }
                })
            })
            .collect()
    }
}

impl<'a, Q: StateIndex, S: Symbol, O: Value> IntoAssigments for &'a MealyMachine<O, Q, S> {
    type AssignmentRef = AssignmentReference<'a, (Q, S), O>;

    type Assignments = std::iter::Map<
        std::collections::hash_map::Iter<'a, (Q, S), O>,
        fn((&'a (Q, S), &'a O)) -> Self::AssignmentRef,
    >;

    fn into_assignments(self) -> Self::Assignments {
        self.acceptance().into_assignments()
    }
}

impl<'a, Q: StateIndex, S: Symbol, O: Value> IntoTransitions for &'a MealyMachine<O, Q, S> {
    type TransitionRef = TransitionReference<'a, Q, S>;

    type IntoTransitions = Transitions<'a, Q, S>;

    fn into_transitions(self) -> Self::IntoTransitions {
        self.ts().into_transitions()
    }
}

impl<Q: StateIndex, S: Symbol, O: Value> MealyMachine<O, Q, S> {
    pub fn mealy_product<Rhs, Q2, O2>(
        &self,
        other: Rhs,
    ) -> MealyMachine<Pair<O, O2>, Pair<Q, Q2>, S>
    where
        Rhs: IntoAssigments + IntoTransitions<Sigma = S, Q = Q2> + Pointed,
        Rhs::AssignmentRef: Assignment<Left = TriggerOf<Rhs>, Right = O2>,
        O2: Value,
        Q2: StateIndex,
    {
        let ts = self.ts().product_with_transitions(other);
        let assignments = self
            .acceptance()
            .product_with_assignments(other)
            .into_assignments()
            .filter_map(|r| {
                let Pair {
                    left: t1,
                    right: t2,
                } = r.left();
                let Pair { left: q, right: p } = r.right();

                if t1.1 == t2.1 {
                    Some(((Pair::new(t1.0, t2.0), t1.1), Pair::new(q, p)))
                } else {
                    None
                }
            })
            .collect();
        let initial = Pair::new(self.initial(), other.initial());
        Combined::from_parts(ts, initial, assignments)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        output::{IntoAssigments, Mapping},
        ts::IntoTransitions,
        MealyMachine, Pair, Transformer, TransitionSystem,
    };

    #[test]
    fn simple_direct_ts_product() {
        let left =
            TransitionSystem::from_iter([(0, 'a', 0), (0, 'b', 1), (1, 'a', 0), (1, 'b', 1)]);
        let right =
            TransitionSystem::from_iter([(0, 'a', 1), (0, 'b', 0), (1, 'a', 0), (1, 'b', 0)]);

        let prod = left.product_with_transitions(&right);
        assert_eq!(prod.size(), 4);

        let edges = prod.into_transitions();
        assert_eq!(edges.count(), 8);
    }

    #[test]
    fn simple_direct_mapping_product() {
        let left: Mapping<char, _> = vec![('a', 0), ('b', 1)].into_iter().collect();
        let right: Mapping<char, _> = vec![('a', 1), ('b', 0)].into_iter().collect();
    }

    #[test]
    fn mealy_machine_product() {
        let mm = MealyMachine::from_iter(
            [
                (0, 'a', 0, 0),
                (0, 'b', 1, 1),
                (1, 'a', 0, 0),
                (1, 'b', 1, 1),
            ],
            0,
        );

        let mm2 = MealyMachine::from_iter(
            [
                (0, 'a', 1, 1),
                (0, 'b', 0, 0),
                (1, 'a', 0, 0),
                (1, 'b', 0, 0),
            ],
            0,
        );

        let prod = mm.mealy_product(&mm2);
        println!("{}", prod);
    }
}
