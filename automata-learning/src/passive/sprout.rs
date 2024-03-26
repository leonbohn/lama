use automata::{
    prelude::*,
    transition_system::{impls::NTState, path},
    Set,
};

use std::collections::HashSet;

use crate::prefixtree::prefix_tree;

use super::{consistency::ConsistencyCheck, Buchi, OmegaSample, Parity};

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
    let thresh = (lb + le ^ 2 + 1) as isize;

    // while there are positive sample words that are escaping
    'outer: while let Some(escape_prefix) =
        length_lexicographical_sort(ts.escape_prefixes(sample.positive_words()).collect()).first()
    {
        let u = escape_prefix[..escape_prefix.len() - 1].to_string();
        let a = escape_prefix.chars().last().expect("empty escape prefix");
        // check thresh
        if (u.len() as isize) - 1 > thresh {
            // compute default automaton
            return acc_type.default_automaton(&sample);
        }
        let source = ts.finite_run(u).unwrap().reached();
        for q in ts.state_indices() {
            // try adding transition
            ts.add_edge(source, a, q, Void);
            // continue if consistent
            if acc_type.consistent(&ts, &sample) {
                continue 'outer;
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
    use crate::passive::{Buchi, OmegaSample, Parity};
    use automata::prelude::*;

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
                (0, 'b', false, 2),
                (2, 'a', false, 2),
                (2, 'b', false, 2),
            ])
            .default_color(Void)
            .deterministic()
            .with_initial(0)
            .into_dba();

        let res = sprout(sample, Buchi);
        assert_eq!(format!("{:?}", res), format!("{:?}", dba));
    }

    #[test]
    fn sprout_buchi_thresh() {
        let sigma = alphabet!(simple 'a', 'b');

        // build sample
        let sample = OmegaSample::new_omega_from_pos_neg(
            sigma,
            [upw!("a"), upw!("baa")],
            [upw!("ab"), upw!("ba"), upw!("babaa"), upw!("baaba")],
        );

        // build dba
        let mut dba = NTS::builder()
            .with_transitions([
                (0, 'a', true, 1),
                (0, 'b', true, 2),
                (1, 'a', true, 1),
                (1, 'b', false, 5),
                (2, 'a', true, 3),
                (2, 'b', false, 5),
                (3, 'a', true, 4),
                (3, 'b', false, 5),
                (4, 'b', true, 2),
                (4, 'a', false, 5),
                (5, 'a', false, 5),
                (5, 'b', false, 5),
            ])
            .default_color(Void)
            .deterministic()
            .with_initial(0)
            .into_dba();

        let res = sprout(sample, Buchi);
        assert_eq!(format!("{:?}", res), format!("{:?}", dba));
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
