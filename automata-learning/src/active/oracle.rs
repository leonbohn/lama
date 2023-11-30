use automata::{prelude::*, ts::operations::MapStateColor, word::LinearWord};

use crate::passive::Sample;

/// A trait that encapsulates a minimally adequate teacher (MAT) for active learning. This is mainly used by
/// L*-esque algorithms and can be implemented by wildly different types, for example an automaton, a function
/// or even a collection of words.
///
/// This trait is designed in a generic way, allowing us to use it for learning a priority mapping, which assigns
/// non-empty finite words a value of type `Output`. This means we can learn a Mealy machine by using priorities as
/// the `Output` type, but it also enables us to learn a regular language/deterministic finite automaton by using
/// `bool` as the `Output` type.
pub trait LStarOracle<H: HasAlphabet> {
    /// The length type of the words that this oracle can handle.
    type Length: Length;
    /// The output type, for a DFA that would be a boolean, but a Mealy Machine might output a priority instead.
    type Output: Color;

    /// Query the desired output for the given word.
    fn output<W: FiniteWord<SymbolOf<H>>>(&self, word: W) -> Self::Output;

    /// Test the given hypothesis for equivalence, returning `Ok(())` if it is equivalent and `Err((word, color))` otherwise.
    /// In the latter case, `word` is a counterexample from the symmetric difference of the target and the hypothesis,
    /// meaning it produces a different output in the hypothesis compared to the target.
    fn equivalence(&self, hypothesis: &H) -> Result<(), (Vec<SymbolOf<H>>, Self::Output)>;
}

/// An oracle/minimally adequate teacher based on a [`Sample`]. It answers membership queries by looking up the
/// word in the sample and returning the corresponding color. If the word is not in the sample, it returns the
/// default color. Equivalence queries are perfomed by checking if the hypothesis produces the same output as the
/// sample for all words in the sample.
#[derive(Debug, Clone)]
pub struct SampleOracle<A: Alphabet, W: LinearWord<A::Symbol>, C: Color> {
    sample: Sample<A, W, C>,
    default: C,
}

impl<A: Alphabet, W: LinearWord<A::Symbol>, C: Color> SampleOracle<A, W, C> {
    /// Returns a reference to the underlying alphabet, as provided by [`Sample::alphabet()`].
    pub fn alphabet(&self) -> &A {
        self.sample.alphabet()
    }
}

impl<A: Alphabet, C: Color> LStarOracle<MooreMachine<A, C>> for SampleOracle<A, Vec<A::Symbol>, C> {
    type Length = FiniteLength;

    type Output = C;

    fn output<V: FiniteWord<SymbolOf<MooreMachine<A, C>>>>(&self, word: V) -> Self::Output {
        self.sample
            .entries()
            .find_map(|(w, c)| {
                if w == &word.to_vec() {
                    Some(c.clone())
                } else {
                    None
                }
            })
            .unwrap_or(self.default.clone())
    }

    fn equivalence(
        &self,
        hypothesis: &MooreMachine<A, C>,
    ) -> Result<(), (Vec<SymbolOf<MooreMachine<A, C>>>, Self::Output)> {
        for (w, c) in self.sample.entries() {
            if hypothesis.reached_state_color(w).as_ref() != Some(c) {
                return Err((w.to_vec(), c.clone()));
            }
        }
        Ok(())
    }
}

impl<A: Alphabet, C: Color> LStarOracle<MealyMachine<A, C>> for SampleOracle<A, Vec<A::Symbol>, C> {
    type Length = FiniteLength;
    type Output = C;
    fn output<W: FiniteWord<SymbolOf<MealyMachine<A, C>>>>(&self, word: W) -> Self::Output {
        self.sample
            .entries()
            .find_map(|(w, c)| {
                if w == &word.to_vec() {
                    Some(c.clone())
                } else {
                    None
                }
            })
            .unwrap_or(self.default.clone())
    }
    fn equivalence(
        &self,
        hypothesis: &MealyMachine<A, C>,
    ) -> Result<(), (Vec<SymbolOf<MealyMachine<A, C>>>, Self::Output)> {
        let Some(cex) = self.sample.entries().find_map(|(w, c)| {
            if hypothesis.last_edge_color(w).as_ref() != Some(c) {
                Some((w.to_vec(), c.clone()))
            } else {
                None
            }
        }) else {
            return Ok(());
        };
        Err(cex)
    }
}

impl<A: Alphabet, W: FiniteWord<A::Symbol>, C: Color> From<(Sample<A, W, C>, C)>
    for SampleOracle<A, W, C>
{
    fn from((value, default): (Sample<A, W, C>, C)) -> Self {
        Self::new(value, default)
    }
}

impl<A: Alphabet, W: FiniteWord<A::Symbol>, C: Color> SampleOracle<A, W, C> {
    /// Creates a new instance of a [`SampleOracle`] with the given sample and default color.
    pub fn new(sample: Sample<A, W, C>, default: C) -> Self {
        Self { sample, default }
    }
}

/// An oracle base on a [`DFALike`] instance. It answers membership queries by running the word through the
/// automaton and returning the result. Equivalence queries are performed by intersecting the hypothesis with
/// the negated input automaton and returning a counterexample if the intersection is non-empty.
#[derive(Debug, Clone)]
pub struct DFAOracle<D: DFALike> {
    automaton: D,
    negated: MapStateColor<D, fn(bool) -> bool>,
}

impl<D: DFALike + Clone> DFAOracle<D> {
    /// Creates a new instance of a [`DFAOracle`] from the given automaton.
    pub fn new(automaton: D) -> Self {
        Self {
            negated: automaton.clone().negation(),
            automaton,
        }
    }
}

impl<D: DFALike> HasAlphabet for DFAOracle<D> {
    type Alphabet = D::Alphabet;
    fn alphabet(&self) -> &Self::Alphabet {
        self.automaton.alphabet()
    }
}

impl<D: DFALike> LStarOracle<MooreMachine<D::Alphabet, bool>> for DFAOracle<D> {
    type Length = FiniteLength;

    type Output = bool;

    fn output<W: FiniteWord<SymbolOf<D>>>(&self, word: W) -> Self::Output {
        (&self.automaton).into_dfa().accepts_finite(word)
    }

    fn equivalence(
        &self,
        hypothesis: &MooreMachine<D::Alphabet, bool>,
    ) -> Result<(), (Vec<SymbolOf<Self>>, Self::Output)> {
        let dfa = (&self.negated).intersection(&hypothesis).into_dfa();
        match dfa.dfa_give_word() {
            Some(w) => {
                let should_be_accepted = (&self.automaton).into_dfa().accepts_finite(&w);
                Err((w, should_be_accepted))
            }
            None => Ok(()),
        }
    }
}

/// An oracle based on a [`MealyMachine`].
#[derive(Debug, Clone)]
pub struct MealyOracle<C: Color, D: MealyLike<C>> {
    automaton: D,
    _color: std::marker::PhantomData<C>,
}

impl<C: Color, D: MealyLike<C>> LStarOracle<MealyMachine<D::Alphabet, C>> for MealyOracle<C, D> {
    type Length = FiniteLength;

    type Output = C;

    fn output<W: FiniteWord<SymbolOf<D>>>(&self, word: W) -> Self::Output {
        self.automaton
            .try_mealy_map(word)
            .expect("The oracle must be total!")
    }

    fn equivalence(
        &self,
        hypothesis: &MealyMachine<D::Alphabet, C>,
    ) -> Result<(), (Vec<SymbolOf<Self>>, Self::Output)> {
        todo!()
    }
}

impl<C: Color, D: MealyLike<C>> HasAlphabet for MealyOracle<C, D> {
    type Alphabet = D::Alphabet;
    fn alphabet(&self) -> &Self::Alphabet {
        self.automaton.alphabet()
    }
}

impl<C: Color, D: MealyLike<C>> MealyOracle<C, D> {
    /// Creates a new [`MealyOracle`] based on an instance of [`MealyLike`].
    pub fn new(automaton: D) -> Self {
        Self {
            automaton,
            _color: std::marker::PhantomData,
        }
    }
}

/// An oracle based on a [`MooreMachine`].
#[derive(Debug, Clone)]
pub struct MooreOracle<C: Color, D: MooreLike<C>> {
    automaton: D,
    _color: std::marker::PhantomData<C>,
}

impl<C: Color, D: MooreLike<C>> LStarOracle<MooreMachine<D::Alphabet, C>> for MooreOracle<C, D> {
    type Length = FiniteLength;

    type Output = C;

    fn output<W: FiniteWord<SymbolOf<D>>>(&self, word: W) -> Self::Output {
        self.automaton
            .try_moore_map(word)
            .expect("The oracle must be total!")
    }

    fn equivalence(
        &self,
        hypothesis: &MooreMachine<D::Alphabet, C>,
    ) -> Result<(), (Vec<SymbolOf<Self>>, Self::Output)> {
        todo!()
    }
}

impl<C: Color, D: MooreLike<C>> HasAlphabet for MooreOracle<C, D> {
    type Alphabet = D::Alphabet;
    fn alphabet(&self) -> &Self::Alphabet {
        self.automaton.alphabet()
    }
}

impl<C: Color, D: MooreLike<C>> MooreOracle<C, D> {
    /// Creates a new [`MooreOracle`] based on an instance of [`MooreLike`].
    pub fn new(automaton: D) -> Self {
        Self {
            automaton,
            _color: std::marker::PhantomData,
        }
    }
}
