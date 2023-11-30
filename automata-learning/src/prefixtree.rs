use std::collections::VecDeque;

use automata::{
    ts::Sproutable, word::OmegaWord, Alphabet, HasLength, InfiniteLength, Map, Pointed,
    RightCongruence, Set,
};
use itertools::Itertools;
use tracing::trace;

use automata::word::Reduced;

pub fn prefix_tree<A: Alphabet, W: Into<Reduced<A::Symbol>>, I: IntoIterator<Item = W>>(
    alphabet: A,
    words: I,
) -> RightCongruence<A> {
    let words: Vec<Reduced<_>> = words.into_iter().map(|word| word.into()).collect_vec();
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
            tree.add_edge(current, A::expression(*symbol), next, ());
            current = next;
        }
        tree.add_edge(
            current,
            A::expression(loop_segment[loop_segment.len() - 1]),
            state,
            (),
        );
    }
    let mut tree = RightCongruence::new(alphabet.clone());
    let root = tree.initial();

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
                    tree.add_edge(state, A::expression(sym), successor, ());
                    queue.push_back((successor, new_access, new_words.into_iter().collect()));
                }
            }
        }
    }

    tree
}
