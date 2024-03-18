use automata::{
    automaton::Buchi,
    prelude::*,
    ts::{nts::NTState, path},
    Set,
};

use std::collections::HashSet;

use super::{consistency::ConsistencyCheck, OmegaSample};

/// gives a deterministic acc_type omega automaton that is consistent with the given sample
/// implements the sprout passive learning algorithm for omega automata from <https://arxiv.org/pdf/2108.03735.pdf>
pub fn sprout<A: ConsistencyCheck<Initialized<DTS>>>(sample: OmegaSample, acc_type: A) -> A::Aut {
    // make ts with initial state
    let mut ts = DTS::from_parts(sample.alphabet().clone(), vec![NTState::new(Void)], vec![])
        .with_initial(0);

    // compute threshold
    let (lb, le) = sample
        .words()
        .map(|w| (w.spoke().len(), w.cycle().len()))
        .fold((0, 0), |(a0, a1), (b0, b1)| (a0.max(b0), a1.max(b1)));
    let thresh = lb + le ^ 2 + 1;

    // while there are positive sample words that are escaping
    while let Some(escape_prefix) = escape_prefixes(&ts, sample.positive_words().collect()).first()
    {
        let u = escape_prefix[..escape_prefix.len() - 1].to_string();
        let a = escape_prefix.chars().last().expect("empty escape prefix");
        // check thresh
        if u.len() - 1 > thresh {
            // compute default automaton
            todo!()
        }
        let source = ts.finite_run(u).unwrap().reached();
        for q in ts.state_indices() {
            // try adding transition
            ts.add_edge(source, a, q, Void);
            // continue if consistent
            if acc_type.consistent(&ts, &sample) {
                continue;
            } else {
                ts.remove_edges(source, a);
            }
        }
        // if none consistent add new state
        let new_state = ts.add_state(Void);
        ts.add_edge(source, a, new_state, Void);
    }
    acc_type.consistent_automaton(&ts, &sample)
}

/// Compute the escape prefixes of a set of omega words on a transition system.
/// The returned vector is sorted length lexicographically
pub fn escape_prefixes<T: TransitionSystem<Alphabet = CharAlphabet> + Deterministic + Pointed>(
    ts: &T,
    words: HashSet<&ReducedOmegaWord<char>>,
) -> Vec<String> {
    let prefixes: HashSet<String> = words
        .into_iter()
        .filter_map(|w| {
            ts.omega_run(w)
                .err()
                .map(|path| w.prefix(path.len() + 1).as_string())
        })
        .collect();
    length_lexicographical_sort(Vec::from_iter(prefixes))
}

/// sort a vector of Strings length lexicographically
pub fn length_lexicographical_sort(mut words: Vec<String>) -> Vec<String> {
    words.sort_by(|a, b| {
        // Compare by length first
        let length_comparison = a.len().cmp(&b.len());

        // If lengths are equal, compare lexicographically
        if length_comparison == std::cmp::Ordering::Equal {
            a.cmp(b)
        } else {
            length_comparison
        }
    });
    words
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

        assert!(sprout(sample, Buchi).eq(&dba));
    }

    #[test]
    fn escapes() {
        // build set of words
        let words = HashSet::from([&upw!("a"), &upw!("a", "b"), &upw!("b"), &upw!("aa", "b")]);

        // build transition system
        let ts = NTS::builder()
            .with_transitions([(0, 'a', Void, 1), (1, 'b', Void, 1)])
            .default_color(Void)
            .deterministic()
            .with_initial(0);

        assert_eq!(
            escape_prefixes(&ts, words),
            vec![String::from("b"), String::from("aa")]
        );
    }

    #[test]
    fn llex_sort() {
        assert_eq!(
            length_lexicographical_sort(vec![
                String::from("ca"),
                String::from("ac"),
                String::from("aaa"),
                String::from("b")
            ]),
            vec![
                String::from("b"),
                String::from("ac"),
                String::from("ca"),
                String::from("aaa")
            ]
        );
    }
}
