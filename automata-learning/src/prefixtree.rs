use std::collections::VecDeque;

use automata::{
    transition_system::Sproutable, word::OmegaWord, Alphabet, Map, Pointed, RightCongruence, Set,
    Void,
};
use itertools::Itertools;
use tracing::trace;

use automata::word::ReducedOmegaWord;

pub fn prefix_tree<A: Alphabet, W: Into<ReducedOmegaWord<A::Symbol>>, I: IntoIterator<Item = W>>(
    alphabet: A,
    words: I,
) -> RightCongruence<A> {
    let words: Vec<ReducedOmegaWord<_>> = words.into_iter().map(|word| word.into()).collect_vec();
    debug_assert!(words.iter().all(|word| !word.raw_word().is_empty()));
    fn build_accepting_loop<A: Alphabet>(
        tree: &mut RightCongruence<A>,
        state: usize,
        access: Vec<A::Symbol>,
        loop_segment: &[A::Symbol],
    ) {
        let mut access = access;
        let mut current = state;
        for symbol in &loop_segment[..loop_segment.len() - 1] {
            access.push(*symbol);
            trace!("adding state {:?}", access);
            let next = tree.add_state(access.clone());
            tree.add_edge(current, A::expression(*symbol), next, Void);
            current = next;
        }
        tree.add_edge(
            current,
            A::expression(loop_segment[loop_segment.len() - 1]),
            state,
            Void,
        );
    }
    let mut tree = RightCongruence::new(alphabet.clone());
    let root = tree.add_state((vec![], Void));

    let mut queue = VecDeque::from_iter([(root, vec![], words.to_vec())]);

    while let Some((state, access, words)) = queue.pop_front() {
        debug_assert!(!words.is_empty());
        debug_assert!(words.iter().all(|word| !word.raw_word().is_empty()));
        if words.len() == 1 && words[0].loop_index() == 0 {
            build_accepting_loop(&mut tree, state, access, words[0].cycle());
        } else {
            let mut map: Map<_, Set<_>> = Map::default();
            for mut word in words {
                let sym = word.pop_front();
                debug_assert!(
                    !word.raw_word().is_empty(),
                    "popping front lead to empty representation"
                );

                map.entry(sym).or_default().insert(word);
            }

            for sym in alphabet.universe() {
                if let Some(new_words) = map.remove(&sym) {
                    debug_assert!(!new_words.is_empty());
                    let new_access = access
                        .iter()
                        .cloned()
                        .chain(std::iter::once(sym))
                        .collect_vec();
                    trace!("Adding state {:?}", new_access);
                    let successor = tree.add_state(new_access.clone());
                    tree.add_edge(state, A::expression(sym), successor, Void);
                    queue.push_back((successor, new_access, new_words.into_iter().collect()));
                }
            }
        }
    }

    tree
}

#[cfg(test)]
mod tests {
    use automata::{
        alphabet::CharAlphabet,
        transition_system::{Deterministic, Dottable, Sproutable},
        upw,
        word::PeriodicOmegaWord,
        TransitionSystem, Void,
    };

    use super::prefix_tree;

    #[test]
    fn build_prefix_tree() {
        let words = [upw!("aa"), upw!("aba"), upw!("bbaab"), upw!("bb")];
        let alphabet = CharAlphabet::from_iter(['a', 'b']);
        let pta = prefix_tree(alphabet, words);
        let completed = pta
            .erase_state_colors()
            .collect_complete_with_initial(Void, Void);
        let lead_to_sink = ["ba", "bbbbbbbbba", "ababababbbabaababa", "aaaaaaaaaaaaab"];
        for w in &lead_to_sink {
            for v in &lead_to_sink {
                assert_eq!(
                    completed.reached_state_index(w),
                    completed.reached_state_index(v)
                );
            }
        }
    }
}
