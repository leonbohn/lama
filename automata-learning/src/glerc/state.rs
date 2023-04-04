use std::hash::Hash;

use automata::{
    run::{Configuration, EscapePrefix, Evaluate},
    Subword, Symbol,
};

use crate::{
    forcs::{Class, RightCongruence},
    sample::Sample,
};

/// Holds the current state of the GLERC algorithm.
/// - `S` is the type of the symbols of the words in the sample
/// - `W` is the type of the words in the sample
pub struct GlercState<'s, S: Symbol, W: Subword<S = S>> {
    /// The congruence constructed thus far
    pub(crate) cong: RightCongruence<S>,
    /// The default congruence
    pub(crate) default: RightCongruence<S>,
    /// The sample
    pub(crate) sample: &'s Sample<W>,
    /// The current iteration
    pub(crate) iteration: usize,
}

/// Alias for the result of evaluating a configuration
type EvalResult<'s, S, W> = <Configuration<&'s RightCongruence<S>, &'s W> as Evaluate>::Output;
/// Alias for the induced pair
type InducedPair<'s, S, W> = (Vec<EvalResult<'s, S, W>>, Vec<EvalResult<'s, S, W>>);

impl<'s, S, W> GlercState<'s, S, W>
where
    S: Symbol + 's,
    W: Subword<S = S> + Clone + 's + Hash,
    Configuration<&'s RightCongruence<S>, &'s W>: Evaluate<Failure = EscapePrefix<Class<S>, &'s W>>,
{
    /// Creates a new state for the given sample and default congruence
    pub fn new(sample: &'s Sample<W>, default: RightCongruence<S>) -> Self {
        let cong = RightCongruence::empty_trivial();
        Self {
            cong,
            default,
            sample,
            iteration: 0,
        }
    }

    /// Returns the next missing transition, if any
    pub fn next_missing(&'s self) -> Option<(Class<S>, S)> {
        self.sample
            .iter()
            .filter_map(
                |word| match Configuration::for_pointed(&self.cong, word).evaluate() {
                    Ok(_) => None,
                    Err(escape_prefix) => Some(escape_prefix),
                },
            )
            .min()
            .map(|ep| ep.trigger())
    }

    /// Returns the induced pair for the current congruence
    pub fn induced(&'s self) -> InducedPair<'s, S, W> {
        (
            self.sample
                .positive_iter()
                .filter_map(|word| Configuration::for_pointed(&self.cong, word).evaluate().ok())
                .collect(),
            self.sample
                .negative_iter()
                .filter_map(|word| Configuration::for_pointed(&self.cong, word).evaluate().ok())
                .collect(),
        )
    }
}

#[cfg(test)]
mod tests {
    use automata::Growable;

    use super::*;
    use crate::{forcs::RightCongruence, glerc::tests::sample_two};

    #[test]
    fn test_missings() {
        let sample = sample_two();
        let mut glercstate = GlercState::new(&sample, RightCongruence::empty_trivial());
        let eps = Class::epsilon();
        let a = Class::from('a');
        let mut expected_induced = (vec![], vec![eps.clone()]);

        assert_eq!(glercstate.next_missing(), Some((eps.clone(), 'a')));
        assert_eq!(glercstate.induced(), expected_induced);

        glercstate.cong.add_state(&a);
        glercstate.cong.add_transition(&eps, 'a', &a);
        expected_induced.0.push(a.clone());
        assert_eq!(glercstate.next_missing(), Some((eps.clone(), 'b')));
        assert_eq!(glercstate.induced(), expected_induced);

        glercstate.cong.add_transition(&eps, 'b', &eps);
        expected_induced.1.push(eps.clone());
        assert_eq!(glercstate.next_missing(), Some((a.clone(), 'a')));
        assert_eq!(glercstate.induced(), expected_induced);

        glercstate.cong.add_transition(&a, 'a', &eps);
        expected_induced.1.push(eps.clone());
        assert_eq!(glercstate.next_missing(), Some((a.clone(), 'b')));
        assert_eq!(glercstate.induced(), expected_induced);

        glercstate.cong.add_transition(&a, 'b', &a);
        expected_induced.0.push(a.clone());
        assert_eq!(glercstate.next_missing(), None);
        assert_eq!(glercstate.induced(), expected_induced);
    }
}
