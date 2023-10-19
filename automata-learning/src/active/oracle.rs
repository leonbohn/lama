use automata::{automata::MooreLike, prelude::*, ts::operations::MapStateColor};

/// A trait that encapsulates a minimally adequate teacher (MAT) for active learning. This is mainly used by
/// L*-esque algorithms and can be implemented by wildly different types, for example an automaton, a function
/// or even a collection of words.
///
/// This trait is designed in a generic way, allowing us to use it for learning a priority mapping, which assigns
/// non-empty finite words a value of type `Output`. This means we can learn a Mealy machine by using priorities as
/// the `Output` type, but it also enables us to learn a regular language/deterministic finite automaton by using
/// `bool` as the `Output` type.
pub trait Oracle: HasAlphabet {
    /// The length type of the words that this oracle can handle.
    type Length: Length;
    /// The output type, for a DFA that would be a boolean, but a Mealy Machine might output a priority instead.
    type Output: Color;

    /// Query the desired output for the given word.
    fn output<W: Word<Symbol = SymbolOf<Self>, Length = Self::Length>>(
        &self,
        word: W,
    ) -> Self::Output;

    /// Test the given hypothesis for equivalence, returning `Ok(())` if it is equivalent and `Err((word, color))` otherwise.
    /// In the latter case, `word` is a counterexample from the symmetric difference of the target and the hypothesis,
    /// meaning it produces a different output in the hypothesis compared to the target.
    fn equivalence<H>(&self, hypothesis: H) -> Result<(), (Vec<SymbolOf<Self>>, Self::Output)>
    where
        H: MooreLike<Self::Output, Alphabet = Self::Alphabet>;
}

#[derive(Debug, Clone)]
pub struct DFAOracle<D: DFALike> {
    automaton: D,
    negated: MapStateColor<D, fn(bool) -> bool>,
}

impl<D: DFALike + Clone> DFAOracle<D> {
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

impl<D: DFALike> Oracle for DFAOracle<D> {
    type Length = FiniteLength;

    type Output = bool;

    fn output<W: Word<Symbol = SymbolOf<Self>, Length = Self::Length>>(
        &self,
        word: W,
    ) -> Self::Output {
        (&self.automaton).into_dfa().accepts(word)
    }

    fn equivalence<H>(&self, hypothesis: H) -> Result<(), (Vec<SymbolOf<Self>>, Self::Output)>
    where
        H: MooreLike<Self::Output, Alphabet = Self::Alphabet>,
    {
        let dfa = (&self.negated).intersection(&hypothesis).into_dfa();
        match dfa.dfa_give_word() {
            Some(w) => {
                let should_be_accepted = (&self.automaton).into_dfa().accepts(&w);
                Err((w, should_be_accepted))
            }
            None => Ok(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MealyOracle<C: Color, D: MealyLike<C>> {
    automaton: D,
    _color: std::marker::PhantomData<C>,
}

impl<C: Color, D: MealyLike<C>> Oracle for MealyOracle<C, D> {
    type Length = FiniteLength;

    type Output = C;

    fn output<W: Word<Symbol = SymbolOf<Self>, Length = Self::Length>>(
        &self,
        word: W,
    ) -> Self::Output {
        self.automaton
            .try_mealy_map(word)
            .expect("The oracle must be total!")
    }

    fn equivalence<H>(&self, hypothesis: H) -> Result<(), (Vec<SymbolOf<Self>>, Self::Output)>
    where
        H: MooreLike<Self::Output, Alphabet = Self::Alphabet>,
    {
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
    pub fn new(automaton: D) -> Self {
        Self {
            automaton,
            _color: std::marker::PhantomData,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MooreOracle<C: Color, D: MooreLike<C>> {
    automaton: D,
    _color: std::marker::PhantomData<C>,
}

impl<C: Color, D: MooreLike<C>> Oracle for MooreOracle<C, D> {
    type Length = FiniteLength;

    type Output = C;

    fn output<W: Word<Symbol = SymbolOf<Self>, Length = Self::Length>>(
        &self,
        word: W,
    ) -> Self::Output {
        self.automaton
            .try_moore_map(word)
            .expect("The oracle must be total!")
    }

    fn equivalence<H>(&self, hypothesis: H) -> Result<(), (Vec<SymbolOf<Self>>, Self::Output)>
    where
        H: MooreLike<Self::Output, Alphabet = Self::Alphabet>,
    {
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
    pub fn new(automaton: D) -> Self {
        Self {
            automaton,
            _color: std::marker::PhantomData,
        }
    }
}
