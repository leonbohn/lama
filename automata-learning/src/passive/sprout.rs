use automata::{automaton::Buchi, prelude::*, ts::path, Set};

use std::collections::HashSet;

use super::OmegaSample;

/// This trait represents the sprout passive learning algorithm for omega automata
/// from <https://arxiv.org/pdf/2108.03735.pdf>.
/// The trait is supposed to be implemented on omega acceptance types
pub trait SproutLearner {
    /// type of the automaton to be returned
    type Aut;

    /// gives a deterministic omega automaton that is consistent with the given sample
    fn sprout(&self, sample: OmegaSample) -> Self::Aut;
}

impl SproutLearner for Buchi {
    type Aut = DBA;

    fn sprout(&self, sample: OmegaSample) -> Self::Aut {
        // make empty ts
        let mut ts = DBA::new_for_alphabet(sample.alphabet());
        ts.add_state(Void);

        // compute escapes
        // while escapes left
        //      get first escape prefix
        //      check thresh
        //      for each state
        //          try adding transition
        //          continue if consistent
        //      if none consistent add new state
        todo!()
    }
}

/// Compute the escape prefixes of a set of omega words on a transition system
pub fn escape_prefixes<T: TransitionSystem<Alphabet = CharAlphabet> + Deterministic + Pointed>(
    ts: T,
    words: &HashSet<ReducedOmegaWord<char>>,
) -> HashSet<String> {
    words
        .into_iter()
        .filter_map(|w| {
            ts.omega_run(w)
                .err()
                .map(|path| w.prefix(path.len() + 1).as_string())
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;
    use crate::passive::OmegaSample;
    use automata::{automaton::Buchi, prelude::*};

    #[test]
    fn sprout_buchi() {
        let sigma = alphabet!(simple 'a', 'b');

        // build sample
        let sample =
            OmegaSample::new_omega_from_pos_neg(sigma, [upw!("a"), upw!("a", "b")], [upw!("b")]);

        // build dba
        let dba = NTS::builder()
            .with_transitions([
                (0, 'a', true, 1),
                (1, 'a', true, 0),
                (1, 'b', true, 1),
                (0, 'b', false, 0),
            ])
            .default_color(Void)
            .deterministic()
            .with_initial(0)
            .into_dba();

        assert!(Buchi.sprout(sample).eq(&dba))
    }

    #[test]
    fn escapes() {
        // build set of words
        let words = HashSet::from([upw!("a"), upw!("a", "b"), upw!("b"), upw!("aa", "b")]);

        // build transition system
        let ts = NTS::builder()
            .with_transitions([(0, 'a', Void, 1), (1, 'b', Void, 1)])
            .default_color(Void)
            .deterministic()
            .with_initial(0);

        let escapes = HashSet::from([String::from("aa"), String::from("b")]);

        assert_eq!(escape_prefixes(ts, &words), escapes);
    }
}
