use automata::{Acceptor, Class, Dfa, Map, Pointed, StateIndex, Symbol, Transducer};

/// A trait that encapsulates a minimally adequate teacher (MAT) for active learning. This is mainly used by
/// L*-esque algorithms and can be implemented by wildly different types, for example an automaton, a function
/// or even a collection of words.
///
/// This trait is designed in a generic way, allowing us to use it for learning a priority mapping, which assigns
/// non-empty finite words a value of type `Output`. This means we can learn a Mealy machine by using [`Priority`] as
/// the `Output` type, but it also enables us to learn a regular language/deterministic finite automaton by using
/// `bool` as the `Output` type.
pub trait Oracle {
    /// The input symbol type for the oracle, it receives words over this alphabet.
    type Input: Symbol;
    /// The output type, for a DFA that would be a boolean, but a Mealy Machine might output a [`Priority`] instead.
    type Output: Symbol;

    /// Query the desired output for the given word.
    fn output(&mut self, word: &Class<Self::Input>) -> Self::Output;

    /// Test the given hypothesis for equivalence, returning `Ok(())` if it is equivalent and `Err(word)` otherwise.
    /// In the latter case, `word` is a counterexample from the symmetric difference of the target and the hypothesis,
    /// meaning it produces a different output in the hypothesis compared to the target.
    fn equivalence<M: Pointed + Transducer<Input = Self::Input, Output = Self::Output>>(
        &mut self,
        hypothesis: &M,
    ) -> Result<(), Class<Self::Input>>;
}

impl<Q: StateIndex, S: Symbol> Oracle for Dfa<Q, S> {
    type Input = S;
    type Output = bool;

    fn output(&mut self, word: &Class<S>) -> bool {
        self.accepts(word)
    }

    fn equivalence<M: Transducer>(&mut self, hypothesis: &M) -> Result<(), Class<S>> {
        todo!()
    }
}

/// Wraps a given oracle and memorizes the output and equivalence queries it receives. Also stores
/// the output of these queries.
#[derive(Debug, Clone)]
pub struct MemorizingOracle<O: Oracle> {
    /// The oracle that is being wrapped.
    oracle: O,
    /// A cache of the queries that have been made to the oracle.
    cache: Map<Class<O::Input>, O::Output>,
}

impl<O: Oracle> MemorizingOracle<O> {
    /// Takes an oracle and wraps it, returning a new oracle that memorizes the queries it receives.
    pub fn new(oracle: O) -> Self {
        Self {
            oracle,
            cache: Map::new(),
        }
    }
}

impl<O: Oracle> Oracle for MemorizingOracle<O> {
    type Input = O::Input;

    type Output = O::Output;

    fn output(&mut self, word: &Class<Self::Input>) -> Self::Output {
        // Can see if the cache already contains the word.
        if let Some(result) = self.cache.get(word) {
            return result.clone();
        }
        let result = self.oracle.output(word);
        self.cache.insert(word.clone(), result.clone());
        result
    }

    fn equivalence<M: Pointed + Transducer<Input = Self::Input, Output = Self::Output>>(
        &mut self,
        hypothesis: &M,
    ) -> Result<(), Class<Self::Input>> {
        match self.oracle.equivalence(hypothesis) {
            Ok(()) => Ok(()),
            Err(word) => {
                // query the for the counterexample and store it
                let result = self.oracle.output(&word);
                self.cache.insert(word.clone(), result);
                Err(word)
            }
        }
    }
}
