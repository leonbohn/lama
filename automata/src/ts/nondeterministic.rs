use crate::prelude::*;

#[derive(Clone)]
pub struct NTS<A: Alphabet, Q, C> {
    alphabet: A,
    adjacency: Vec<(Q, Vec<(A::Expression, usize, C)>)>,
}

impl<Q, C> NTS<Simple, Q, C> {
    pub fn builder() -> NTSBuilder<Q, C> {
        NTSBuilder::new()
    }
}

impl<A: Alphabet, Q, C> HasAlphabet for NTS<A, Q, C> {
    type Alphabet = A;
    fn alphabet(&self) -> &Self::Alphabet {
        &self.alphabet
    }
}

impl<A: Alphabet, Q: Color, C: Color> TransitionSystem for NTS<A, Q, C> {
    type StateIndex = usize;

    type StateColor = Q;

    type EdgeColor = C;

    type TransitionRef<'this> = &'this (A::Expression, usize, Self::EdgeColor)
    where
        Self: 'this;

    type EdgesFromIter<'this> = std::slice::Iter<'this, (A::Expression, usize, C)>
    where
        Self: 'this;

    type StateIndices<'this> = std::ops::Range<usize>
    where
        Self: 'this;

    fn state_indices(&self) -> Self::StateIndices<'_> {
        (0..self.adjacency.len())
    }

    fn edges_from<Idx: Indexes<Self>>(&self, state: Idx) -> Option<Self::EdgesFromIter<'_>> {
        Some(self.adjacency.get(state.to_index(self)?)?.1.iter())
    }

    fn state_color(&self, state: Self::StateIndex) -> Option<Self::StateColor> {
        Some(self.adjacency.get(state.to_index(self)?)?.0.clone())
    }
}

pub struct NTSBuilder<Q, C> {
    edges: Vec<(usize, char, C, usize)>,
    default: Option<Q>,
    colors: Vec<(usize, Q)>,
}

impl<Q, C> NTSBuilder<Q, C> {
    pub fn new() -> Self {
        Self {
            edges: vec![],
            default: None,
            colors: vec![],
        }
    }
    pub fn default_color(mut self, color: Q) -> Self {
        self.default = Some(color);
        self
    }
    pub fn color(mut self, idx: usize, color: Q) -> Self
    where
        Q: Color,
    {
        assert!(self.colors.iter().all(|(q, c)| q != &idx || c == &color));
        self.colors.push((idx, color));
        self
    }
    pub fn extend<T: IntoIterator<Item = (usize, char, C, usize)>>(mut self, iter: T) -> Self {
        self.edges.extend(iter);
        self
    }

    pub fn with_initial(mut self, initial: usize) -> WithInitial<NTS<Simple, Q, C>>
    where
        Q: Color,
        C: Color,
    {
        let alphabet = Simple::from_iter(self.edges.iter().map(|(_, a, _, _)| a.clone()));
        let num_states = *self
            .edges
            .iter()
            .map(|(q, _, _, p)| std::cmp::max(q, p))
            .max()
            .expect("At least one state must exist");
        assert!(
            initial < num_states,
            "Cannot use state as initial which does not exist"
        );
        let mut adjacency = Vec::with_capacity(num_states);
        for outer_source in 0..=num_states {
            let color = self
                .colors
                .iter()
                .find_map(|(p, c)| {
                    if p == &outer_source {
                        Some(c.clone())
                    } else {
                        None
                    }
                })
                .unwrap_or_else(|| {
                    self.default
                        .clone()
                        .expect("Default must be known if no color is provided")
                });
            let edges = self
                .edges
                .iter()
                .filter_map(|(source, a, c, q)| {
                    if source == &outer_source {
                        Some((*a, *q, c.clone()))
                    } else {
                        None
                    }
                })
                .collect();
            adjacency.push((color, edges));
        }

        let nts = NTS {
            alphabet,
            adjacency,
        };
        nts.with_initial(initial)
    }
}
