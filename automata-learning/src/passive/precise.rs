use automata::{
    prelude::{Expression, HasAlphabet, IsTransition, DFA},
    ts::FiniteState,
    Alphabet, Map, Pointed, RightCongruence, TransitionSystem,
};
use itertools::Itertools;

pub type ClassId = usize;
pub type StateId = usize;

#[derive(Copy, Clone)]
pub struct PState<const N: usize> {
    class: usize,
    progress_classes: [usize; N],
    progress_states: [usize; N],
}

pub struct PreciseDPAState {
    class: ClassId,
    progress: Vec<(ClassId, StateId)>,
}

impl<const N: usize> PState<N> {
    pub fn class(&self) -> ClassId {
        self.class
    }

    pub fn progress_classes(&self) -> impl Iterator<Item = ClassId> + '_ {
        self.progress_classes.iter().cloned()
    }

    pub fn progress_states(&self) -> impl Iterator<Item = StateId> + '_ {
        self.progress_states.iter().cloned()
    }

    pub fn from_iters<I: IntoIterator<Item = ClassId>, J: IntoIterator<Item = StateId>>(
        leading: ClassId,
        pc: I,
        pq: J,
    ) -> Self {
        let mut c = [0; N];
        let mut q = [0; N];
        pc.into_iter().enumerate().for_each(|(i, o)| c[i] = o);
        pq.into_iter().enumerate().for_each(|(i, o)| q[i] = o);
        Self {
            class: leading,
            progress_classes: c,
            progress_states: q,
        }
    }

    pub fn new(leading: ClassId, pc: &[ClassId], pq: &[StateId]) -> Self {
        let c = std::array::from_fn(|i| pc[i]);
        let p = std::array::from_fn(|i| pq[i]);

        Self {
            class: leading,
            progress_classes: c,
            progress_states: p,
        }
    }
}

#[derive(Clone)]
pub struct PreciseDPA<A: Alphabet, const N: usize = 8> {
    states: Vec<PState<N>>,
    cong: RightCongruence<A>,
    /// Nat -> class -> DFA
    dfas: Vec<Vec<DFA<A>>>,
}

pub struct PreciseDPATransition<'a, A: Alphabet> {
    dpa: &'a PreciseDPA<A>,
    source: usize,
    expression: A::Expression,
    target: usize,
    color: usize,
}

impl<'a, A: Alphabet> IsTransition<A::Expression, usize, usize> for PreciseDPATransition<'a, A> {
    fn target(&self) -> usize {
        self.target
    }

    fn color(&self) -> usize {
        self.color
    }

    fn expression(&self) -> &A::Expression {
        &self.expression
    }
}

impl<'a, A: Alphabet> PreciseDPATransition<'a, A> {
    pub fn new(
        dpa: &'a PreciseDPA<A>,
        source: usize,
        expression: A::Expression,
        target: usize,
        color: usize,
    ) -> Self {
        Self {
            dpa,
            source,
            expression,
            target,
            color,
        }
    }
}

pub struct PreciseDPAEdgesFrom<'a, A: Alphabet> {
    dpa: &'a PreciseDPA<A>,
    state: usize,
    it: A::Universe<'a>,
}

impl<'a, A: Alphabet> Iterator for PreciseDPAEdgesFrom<'a, A> {
    type Item = PreciseDPATransition<'a, A>;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.find_map(|o| {
            if let Some(t) = self.dpa.transition(self.state, *o) {
                Some(PreciseDPATransition::new(
                    self.dpa,
                    self.state,
                    t.expression().clone(),
                    t.target(),
                    t.color(),
                ))
            } else {
                None
            }
        })
    }
}

impl<'a, A: Alphabet> PreciseDPAEdgesFrom<'a, A> {
    pub fn new(dpa: &'a PreciseDPA<A>, state: usize) -> Self {
        Self {
            dpa,
            state,
            it: dpa.alphabet().universe(),
        }
    }
}

impl<A: Alphabet> TransitionSystem for PreciseDPA<A> {
    type StateIndex = usize;

    type StateColor = ();

    type EdgeColor = usize;

    type TransitionRef<'this> = PreciseDPATransition<'this, A>
    where
        Self: 'this;

    type EdgesFromIter<'this> = PreciseDPAEdgesFrom<'this, A>
    where
        Self: 'this;

    fn transition(
        &self,
        state: Self::StateIndex,
        symbol: automata::prelude::SymbolOf<Self>,
    ) -> Option<Self::TransitionRef<'_>> {
        todo!()
    }

    fn edge_color(
        &self,
        state: Self::StateIndex,
        expression: &automata::prelude::ExpressionOf<Self>,
    ) -> Option<automata::ts::EdgeColor<Self>> {
        let symbols = expression.symbols().collect::<Vec<_>>();
        assert_eq!(
            symbols.len(),
            1,
            "Only works for alphabets where expressions and symbols coincide"
        );
        let sym = *symbols.first().unwrap();
        todo!()
    }

    fn edges_from(&self, state: Self::StateIndex) -> Option<Self::EdgesFromIter<'_>> {
        Some(PreciseDPAEdgesFrom::new(self, state))
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        Some(())
    }
}

impl<A: Alphabet> HasAlphabet for PreciseDPA<A> {
    type Alphabet = A;
    fn alphabet(&self) -> &Self::Alphabet {
        self.dfas[0][0].alphabet()
    }
}

impl<A: Alphabet, const N: usize> PreciseDPA<A, N> {
    pub fn new(cong: RightCongruence<A>, dfas: Vec<Vec<DFA<A>>>) -> Self {
        assert_eq!(
            dfas.iter().map(|o| o.len()).unique().collect::<Vec<_>>(),
            vec![cong.size()]
        );
        let e = cong.initial();
        let initial = PState::from_iters(e, [e; N], (0..dfas.len()).map(|i| dfas[i][e].initial()));
        Self {
            states: vec![initial],
            cong,
            dfas,
        }
    }

    /// Returns a reference to the leading congruence.
    pub fn cong(&self) -> &RightCongruence<A> {
        &self.cong
    }

    pub fn dfas<'a>(&'a self, q: &'a PState<N>) -> impl Iterator<Item = &DFA<A>> + 'a {
        q.progress_classes()
            .enumerate()
            .map(move |(i, c)| &self.dfas[i][c])
    }

    pub fn take_precise_transition(&self, q: &PState<N>, a: A::Symbol) -> (usize, PState<N>) {
        let d = self
            .cong()
            .successor_index(q.class(), a)
            .expect("Leading congruence must be complete");

        let progress = self
            .dfas(q)
            .zip(q.progress_classes())
            .zip(q.progress_states())
            .map(|((dfa, c), q)| {
                let p = dfa
                    .successor_index(q, a)
                    .expect("all dfas must be complete");
                let b = dfa
                    .state_color(p)
                    .expect("this state must exist as it is successor");
                (c, p, b)
            })
            .collect_vec();

        let least_accepting = progress
            .iter()
            .position(|(_, _, b)| *b)
            .expect("The last DFA must be accepting!");

        (
            least_accepting,
            PState::from_iters(
                d,
                progress
                    .iter()
                    .enumerate()
                    .map(|(i, (c, p, b))| if i < least_accepting { *c } else { d }),
                progress.iter().map(|(_, p, _)| *p),
            ),
        )
    }
}
