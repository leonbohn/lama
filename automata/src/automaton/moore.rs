use hoars::State;
use itertools::Itertools;
use owo_colors::OwoColorize;
use std::fmt::Debug;

use crate::{prelude::*, Void};

use super::Automaton;

/// Represents the semantics of a Moore machine, it produces the color of the
/// state that is reached during a run on a word. If the input is empty, it
/// produces the color of the initial state.
#[derive(Debug, Clone, Default)]
pub struct MooreSemantics<Q>(std::marker::PhantomData<Q>);

/// A Moore machine is a transition system where each state has an output. Thus, the output
/// of running a Moore machine on a word produces a sequence of outputs, one for each state
/// that is visited. For a word of length `n`, there are `n+1` outputs, note in particular
/// that the empty word produce an output, which is in contrast to [`MealyMachine`]s, where
/// the empty word produces no output.
///
/// Usually, we are interested in the output of the last state that is reached during a run
/// on a word. In case of a deterministic Moore machine, this is the only output that is
/// produced. A [`DFA`] is then a Moore machine, where the colors are `bool`. A word reaches
/// a state and the corresponding `bool` is emitted, where `true` corresponds to an accepted
/// input, whereas `false` represents a rejected input. For infinite runs, we usually
/// consider the colors that are produced infinitely often and base acceptance around them. It
/// is, however, prefered to use a [`MealyMachine`] for this purpose, as for infinite inputs
/// switching to transition-based acceptance is preferable.
pub type MooreMachine<A = CharAlphabet, Q = usize> =
    Automaton<Initialized<DTS<A, Q, Void>>, MooreSemantics<Q>, false>;

/// Helper type that takes a pointed transition system and returns the corresponding
/// [`MooreMachine`], which the ts can be converted into using [`Into::into`].
/// For concrete automaton types such as [`DFA`], the [`IntoDFA`] type can be used to
/// obtain the type of a [`DFA`] for the given ts.
pub type IntoMooreMachine<D> = Automaton<D, MooreSemantics<StateColor<D>>, false>;

impl<D: MooreLike + Deterministic> IntoMooreMachine<D>
where
    StateColor<D>: Color,
{
    /// Returns the unique minimal moore machine that is bisimilar to `self`. This means
    /// for every finite word, the output of `self` and the output of the returned moore
    /// machine is the same. This is done using the Hopcroft and Moore algorithms for
    /// minimizing deterministic finite automata, implemented in the
    /// [`crate::algorithms::moore_partition_refinement`] function.
    pub fn minimize(&self) -> MooreMachine<D::Alphabet, D::StateColor>
    where
        StateColor<D>: Color,
    {
        crate::algorithms::moore_partition_refinement(self)
    }
}

/// Implemented by objects that can be viewed as MooreMachines, i.e. finite transition systems
/// that have usize annotated/outputting states.
pub trait MooreLike: Congruence
where
    StateColor<Self>: Color,
{
    /// Consumes and thereby turns `self` into a [`MooreMachine`].
    fn into_moore(self) -> IntoMooreMachine<Self> {
        Automaton::from_parts(self, MooreSemantics(std::marker::PhantomData))
    }

    /// Pushes the state colors onto the outgoing edges of `self` and collects the resulting
    /// transition system into a new [`MealyMachine`].
    fn push_colors_to_outgoing_edges(&self) -> MealyMachine<Self::Alphabet, Self::StateColor>
    where
        Self::StateColor: Clone,
    {
        self.map_edge_colors_full(|p, a, c, q| {
            self.state_color(p)
                .expect("We know it is reachable and it must be colored")
                .clone()
        })
        .collect_mealy()
    }

    /// Runs the given `input` word in self. If the run is successful, the color of the state that it reaches
    /// is emitted (wrapped in a `Some`). For unsuccessful runs, `None` is returned.
    fn try_moore_map<W: FiniteWord<SymbolOf<Self>>>(&self, input: W) -> Option<Self::StateColor> {
        self.reached_state_color(input)
    }

    /// Obtains a vec containing the possible colors emitted by `self` (without duplicates).
    fn color_range(&self) -> Vec<Self::StateColor>
    where
        StateColor<Self>: Color,
    {
        self.reachable_state_indices()
            .map(|o| {
                self.state_color(o)
                    .expect("We know it is reachable and it must be colored")
            })
            .unique()
            .collect()
    }

    /// Builds a moore machine from a reference to `self`. Note that this allocates a new
    /// transition system, which is a copy of the underlying one.
    fn collect_moore(&self) -> MooreMachine<Self::Alphabet, Self::StateColor>
    where
        Self::StateColor: Color,
    {
        self.erase_edge_colors().collect_pointed().0.into_moore()
    }

    /// Returns true if `self` is bisimilar to `other`, i.e. if the two moore machines
    /// produce the same output for each finite word. This is done by checking whether
    /// [`Self::moore_witness_non_bisimilarity`] returns `None`.
    fn moore_bisimilar<M>(&self, other: M) -> bool
    where
        M: MooreLike<Alphabet = Self::Alphabet, StateColor = Self::StateColor>,
        StateColor<Self>: Color,
    {
        self.moore_witness_non_bisimilarity(other).is_none()
    }

    /// Returns a witness for the non-bisimilarity of `self` and `other`, i.e. a finite word
    /// that produces different outputs in the two moore machines. If the two machines are
    /// bisimilar, `None` is returned.
    fn moore_witness_non_bisimilarity<M>(&self, other: M) -> Option<Vec<SymbolOf<Self>>>
    where
        M: MooreLike<Alphabet = Self::Alphabet, StateColor = Self::StateColor>,
        StateColor<Self>: Color,
    {
        let prod = self.ts_product(other);
        for (mr, idx) in prod.minimal_representatives() {
            let (c, d) = prod.state_color(idx).unwrap();
            if c != d {
                return Some(mr);
            }
        }
        None
    }

    /// Decomposes `self` into a sequence of DFAs, where the i-th DFA accepts all words which
    /// produce a color less than or equal to i.
    fn decompose_dfa(&self) -> Vec<DFA<Self::Alphabet>>
    where
        StateColor<Self>: Color,
    {
        self.color_range()
            .into_iter()
            .sorted()
            .map(|i| self.color_or_below_dfa(i))
            .collect()
    }

    /// Builds a DFA that accepts all words which emit a color less than or equal to `color`.
    fn color_or_below_dfa(&self, color: Self::StateColor) -> DFA<Self::Alphabet>
    where
        StateColor<Self>: Color,
    {
        self.map_state_colors(|o| o <= color)
            .erase_edge_colors()
            .into_dfa()
            .minimized()
            .collect_dfa()
    }
}
impl<Ts: Congruence> MooreLike for Ts where StateColor<Ts>: Color {}
