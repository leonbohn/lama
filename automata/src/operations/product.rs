use std::{borrow::Borrow, sync::Arc};

use owo_colors::OwoColorize;
use tracing::trace;

use crate::{
    helpers::MooreMachine,
    output::{Assignment, AssignmentReference, IntoAssignments, Mapping},
    ts::{
        transitionsystem::{States, Transitions},
        HasInput, HasStates, IntoTransitions, TransitionReference, TriggerOf, StateOf, InputOf, IntoStates, StateReference, IntoParts,
    },
    Acceptor, Combined, IntoMealyTransitions, MealyMachine, Pair, Pointed, StateIndex, Successor,
    Symbol, Transformer, Transition, TransitionSystem, Trigger, Value, DBA, DFA, DPA,
};

pub type AssignmentProductMapping<X, Y, A> =
    Mapping<X, Pair<Y, <<A as IntoAssignments>::AssignmentRef as Assignment>::Right>>;

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
        I: IntoAssignments,
        I::AssignmentRef: Assignment<Left = A, Right = B>,
        A: Value,
        B: Value,
    {
        self.into_assignments()
            .flat_map(|a| {
                assignments.into_assignments().map(move |b| {
                    (
                        Pair::new(a.left(), b.left()),
                        Pair::new(a.right(), b.right()),
                    )
                })
            })
            .collect()
    }
}

/// This can be used to build the product of two objects. If both implement [`Successor`]
/// then the product also implements [`Successor`]. The same holds for [`Transformer`] and
/// [`Pointed`]. Moreover, if both are [`IntoTransitions`] or [`IntoAssignments`] then the
/// product implements the respective trait as well.
#[derive(Clone, Debug)]
pub struct Product<L, R> {
    left: L,
    right: R,
}

impl<L, R> Product<L, R> where L: Successor, R: Successor<Sigma = L::Sigma> {
    /// Creates a new [`Product`] from the given elements.
    pub fn new(left: L, right: R) -> Self {
        Self { left, right }
    }
}

impl<L, R> HasInput for Product<L, R>
where
    L: HasInput,
    R: HasInput<Sigma = L::Sigma>,
{
    type Sigma = L::Sigma;

    type Input<'me> = L::Input<'me> 
    where Self:'me;

    fn raw_input_alphabet_iter(&self) -> Self::Input<'_> {
        self.left.raw_input_alphabet_iter()
    }
}

impl<L, R> HasStates for Product<L, R>
where
    L: HasStates,
    R: HasStates,
{
    type Q = Pair<L::Q, R::Q>;

    fn contains_state<X:Borrow<Self::Q>>(&self,state:X) -> bool {
        self.left.contains_state(state.borrow().left()) && self.right.contains_state(state.borrow().right())
    }
}

impl<L, R> Successor for Product<L, R>
where
    L: Successor,
    R: Successor<Sigma = L::Sigma>,
{
    fn successor<X:Borrow<Self::Q>,Y:Borrow<Self::Sigma>>(&self,from:X,on:Y,) -> Option<Self::Q> {
        let Pair { left, right } = from.borrow();
        let sym = on.borrow();
        self.left
            .successor(left, sym)
            .and_then(|l| self.right.successor(right, sym).map(|r| Pair::new(l, r)))
    }
}

impl<L, R> Pointed for Product<L, R> where L: Pointed, R: Pointed<Sigma = L::Sigma> {
    fn initial(&self) -> Self::Q {
        Pair::new(self.left.initial(), self.right.initial())
    }
}

pub struct ProductTransitions<L, R> where L: Iterator {
    left: L,
    left_element: Option<L::Item>,
    right: R,
    right_backup: R,
}

impl<L, R> Iterator for ProductTransitions<L, R> where
L: Iterator,
R: Iterator + Clone,
L::Item: Clone + Transition,
R::Item: Transition<S = <L::Item as Trigger>::S>,
{
    type Item = (Pair<<L::Item as Trigger>::Q, <R::Item as Trigger>::Q>, <L::Item as Trigger>::S, Pair<<L::Item as Trigger>::Q, <R::Item as Trigger>::Q>); 

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(left_element) = self.left_element.clone() {
            // trace!("left_element: {:?} -{:?}-> ", left_element.source(), left_element.sym());
            let right_element = match self.right.find(|x| x.sym() == left_element.sym()) {
                None => {
                    // trace!("\tno right element found, resetting right iterator and advancing left");
                    self.right = self.right_backup.clone();
                    self.left_element = self.left.next();
                    return self.next();
                }
                Some(t) => {
                    // trace!("\tfound right element: {:?}, {}", t.target(), "left not advanced".green());
                    t
                }
            };
            // trace!("\tEmitting: <{:?},{:?}> -{:?}-> <{:?},{:?}>", left_element.source(), right_element.source(), left_element.sym(), left_element.target(), right_element.target());
            Some((Pair::new(left_element.source().clone(), right_element.source().clone()), left_element.sym().clone(), Pair::new(left_element.target().clone(), right_element.target().clone())))
        } else {
            // trace!("left_element is None");
            None
        }
    }
}

pub fn product_transitions<L, R>(left_transitions: L, right_transitions: R) -> ProductTransitions<L::IntoTransitions, R::IntoTransitions> where
L: IntoTransitions,
R: IntoTransitions<Sigma = L::Sigma>,
R::IntoTransitions: Clone,
{
    let mut left = left_transitions.into_transitions();
    let right = right_transitions.into_transitions();
    ProductTransitions {
        left_element: left.next(),
        left,
        right_backup: right.clone(),
        right,
    }
}


pub struct ProductStates<L, R> where L: Iterator {
    left: L,
    left_element: Option<L::Item>,
    right: R,
    right_backup: R,
}

impl<L, R> Iterator for ProductStates<L, R> where
L: Iterator,
R: Iterator + Clone,
L::Item: Clone + StateReference,
R::Item: StateReference,
{
    type Item = Pair<<L::Item as StateReference>::Q, <R::Item as StateReference>::Q>; 

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(left_element) = self.left_element.clone() {
            let right_element = match self.right.next() {
                Some(t) => t,
                None => {
                    self.right = self.right_backup.clone();
                    self.left_element = self.left.next();
                    return self.next();
                },
            };
            // trace!("\tEmitting: <{:?},{:?}> -{:?}-> <{:?},{:?}>", left_element.source(), right_element.source(), left_element.sym(), left_element.target(), right_element.target());
            Some(Pair::new(left_element.state().clone(), right_element.state().clone()) )
        } else {
            // trace!("left_element is None");
            None
        }
    }
}
impl<'a, L, R> IntoStates for &'a Product<L, R> where L: IntoStates, R: IntoStates<Sigma = L::Sigma>,
R::IntoStates: Clone {
    type StateRef = Pair<L::Q, R::Q>;

    type IntoStates = ProductStates<L::IntoStates, R::IntoStates>;

    fn into_states(self) -> Self::IntoStates {
        let mut left = self.left.into_states();
        let right = self.right.into_states();
        ProductStates {
        left_element: left.next(),
        left,
        right_backup: right.clone(),
        right,
    }
    }
}

impl<'a, L, R> IntoTransitions for &'a Product<L, R> where L: IntoTransitions, R: IntoTransitions<Sigma = L::Sigma>,
R::IntoTransitions: Clone, {
    type TransitionRef = (Pair<StateOf<L>, StateOf<R>>, InputOf<L>, Pair<StateOf<L>, StateOf<R>>);

    type IntoTransitions = ProductTransitions<L::IntoTransitions, R::IntoTransitions>;

    fn into_transitions(self) -> Self::IntoTransitions {
        product_transitions(self.left, self.right)
    }
}

impl<L, R> Transformer for Product<L, R> where L: Transformer, R: Transformer {
    type Domain = Pair<L::Domain, R::Domain>;

    type Range = Pair<L::Range, R::Range>;

    fn apply<X: Borrow<Self::Domain>>(&self, input:X) -> Self::Range {
        let Pair {left, right } = input.borrow();
        Pair::new(self.left.apply(left), self.right.apply(right))
    }
}

pub struct ProductAssignments<L, R> where L: Iterator {
    left: L,
    left_element: Option<L::Item>,
    right: R,
    right_backup: R,
}

impl<'a, L, R> IntoAssignments for &'a Product<L, R> where
L: IntoAssignments,
R: IntoAssignments,
R::Assignments: Clone,
L::AssignmentRef: Clone {
    type AssignmentRef = (Pair<L::Domain, R::Domain>, Pair<L::Range, R::Range>);

    type Assignments = ProductAssignments<L::Assignments, R::Assignments>;

    fn into_assignments(self) -> Self::Assignments {
        let mut left = self.left.into_assignments();
        let right = self.right.into_assignments();
        ProductAssignments {
            left_element: left.next(),
            left,
            right_backup: right.clone(),
            right,
        }
    }
}

impl<L, R> Iterator for ProductAssignments<L, R> where
L: Iterator,
R: Iterator + Clone,
L::Item: Clone + Assignment,
R::Item: Assignment,
{
    type Item = (Pair<<L::Item as Assignment>::Left, <R::Item as Assignment>::Left>, Pair<<L::Item as Assignment>::Right, <R::Item as Assignment>::Right>);

    fn next(&mut self) -> Option<Self::Item> {
        let right_element = match self.right.next() {
            None => {
                self.right = self.right_backup.clone();
                match self.right.next() {
                    None => return None,
                    Some(a) => {
                        self.left_element = self.left.next();
                        a
                    }
                }
            }
            Some(a) => a
        };
        self.left_element.as_ref().map(|a| (Pair::new(a.left(), right_element.left()), Pair::new(a.right(), right_element.right())))
    }
}

impl<'a, Q: StateIndex, S: Symbol, O: Value> IntoAssignments for &'a MealyMachine<O, Q, S> {
    type AssignmentRef = AssignmentReference<'a, (Q, S), O>;

    type Assignments = std::iter::Map<
        std::collections::hash_map::Iter<'a, (Q, S), O>,
        fn((&'a (Q, S), &'a O)) -> Self::AssignmentRef,
    >;

    fn into_assignments(self) -> Self::Assignments {
        self.acceptance().into_assignments()
    }
}

impl<'a, Q: StateIndex, S: Symbol, O: Value> IntoStates for &'a MealyMachine<O, Q, S> {
    type StateRef = &'a StateOf<Self>;

    type IntoStates = States<'a, Q>;

    fn into_states(self) -> Self::IntoStates {
        self.ts.into_states()
    }
}
impl<'a, Q: StateIndex, S: Symbol, O: Value> IntoStates for &'a MooreMachine<O, Q, S> {
    type StateRef = &'a StateOf<Self>;

    type IntoStates = States<'a, Q>;

    fn into_states(self) -> Self::IntoStates {
        self.ts.into_states()
    }
}

impl<'a, Q: StateIndex, S: Symbol, O: Value> IntoTransitions for &'a MealyMachine<O, Q, S> {
    type TransitionRef = TransitionReference<'a, Q, S>;

    type IntoTransitions = Transitions<'a, Q, S>;

    fn into_transitions(self) -> Self::IntoTransitions {
        self.ts().into_transitions()
    }
}

impl<'a, Q: StateIndex, S: Symbol, O: Value> IntoAssignments for &'a MooreMachine<O, Q, S> {
    type AssignmentRef = AssignmentReference<'a, Q, O>;

    type Assignments = std::iter::Map<
        std::collections::hash_map::Iter<'a, Q, O>,
        fn((&'a Q, &'a O)) -> Self::AssignmentRef,
    >;

    fn into_assignments(self) -> Self::Assignments {
        self.acceptance().into_assignments()
    }
}

impl<'a, Q: StateIndex, S: Symbol, O: Value> IntoTransitions for &'a MooreMachine<O, Q, S> {
    type TransitionRef = TransitionReference<'a, Q, S>;

    type IntoTransitions = Transitions<'a, Q, S>;

    fn into_transitions(self) -> Self::IntoTransitions {
        self.ts().into_transitions()
    }
}

type MooreProduct<Q, P, S, A, B> = MooreMachine<Pair<A, B>, Pair<Q, P>, S>;

impl<Q: StateIndex, S: Symbol, A: Value> MooreMachine<A, Q, S> {
    /// Computes the direct product of two Moore machines. This is done by computing
    /// the direct product of both the transition systems and the acceptance mappings.
    /// The result is a Moore machine which has pairs as states and pairs of outputs as
    /// acceptance values.
    pub fn direct_product<P: StateIndex, B: Value, D: Borrow<MooreMachine<B, P, S>>>(
        &self,
        other: D,
    ) -> MooreProduct<Q, P, S, A, B> {
        let product = self.product(other.borrow());
        let ts = product.into_ts();
        let acc = product.collect_mapping();
        let initial = Pair::new(self.initial(), other.borrow().initial());
        Combined::from_parts(ts, initial, acc)
    }
}

type MealyProduct<Q, P, S, A, B> = MealyMachine<Pair<A, B>, Pair<Q, P>, S>;

impl<Q: StateIndex, S: Symbol, A: Value> MealyMachine<A, Q, S> {
    /// Computes the direct product of two Mealy machines. It first computes the direct
    /// product of the transition systems and the acceptance mappings. Subsequently, the
    /// mappings are joined on the common input symbol. This results in a Mealy machine
    /// which has pairs as states and pairs as outputs.
    pub fn direct_product<P: StateIndex, B: Value, D: Borrow<MealyMachine<B, P, S>>>(
        &self,
        other: D,
    ) -> MealyProduct<Q, P, S, A, B> {
        let product = self.product(other.borrow());
        let ts = product.into_ts();
        let acc = product
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
        let initial = Pair::new(self.initial(), other.borrow().initial());
        Combined::from_parts(ts, initial, acc)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        output::{IntoAssignments, Mapping},
        ts::{IntoTransitions, Visitor, IntoParts},
        MealyMachine, Pair, Transformer, TransitionSystem, DFA, Successor,
    };

    #[test]
    fn simple_direct_ts_product() {
        let left =
            TransitionSystem::from_iter([(0, 'a', 0), (0, 'b', 1), (1, 'a', 0), (1, 'b', 1)]);
        let right =
            TransitionSystem::from_iter([(0, 'a', 1), (0, 'b', 0), (1, 'a', 0), (1, 'b', 0)]);

        let prod = left.product(&right).into_ts();
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

        let mealy_prod = mm.direct_product(&mm2);
        println!("{}", mealy_prod);

        let prod = mm.product(&mm2);
        println!("{}", prod.bfs().iter().collect::<TransitionSystem<_>>());
    }
}
