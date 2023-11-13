use automata::{alphabet::Fixed, prelude::*, ts::finite::ReachedState};

type Automata = Vec<MooreMachine<Simple, usize>>;
type Words = Vec<Vec<char>>;
type Loops = Vec<Reduced<char, InfiniteLength>>;
lazy_static::lazy_static! {
static ref DATA: (
    Automata,
    Words,
    Loops,
    ) = data();
}
const BENCH_SIZE: usize = 3;
const RANDOM: [usize; 3] = [3, 2, 7];

fn pseudorandom_sprout(states: usize, n: usize) -> MooreMachine<Simple, usize> {
    let alphabet = alphabet!(simple 'a', 'b', 'c', 'd');

    let mut ts = MooreMachine::new(alphabet, 0);

    for i in 1..states {
        ts.add_state(i);
    }

    let mut c: usize = 0;
    for sym in ['a', 'b', 'c', 'd'] {
        for i in 0..states {
            ts.add_edge(i, sym, c, ());
            c = c.wrapping_add(n) % states;
        }
    }
    ts
}

fn random_automata(rand: &[usize]) -> Vec<MooreMachine<Simple, usize>> {
    let mut automata = vec![];
    for n in rand {
        automata.push(pseudorandom_sprout(BENCH_SIZE, *n));
    }
    automata
}

#[allow(clippy::type_complexity)]
fn data() -> (
    Vec<MooreMachine<Simple, usize>>,
    Vec<Vec<char>>,
    Vec<Reduced<char, InfiniteLength>>,
) {
    let random_automata = random_automata(&RANDOM);
    let words = vec![
        vec!['a', 'c', 'a', 'a', 'b', 'a', 'c'],
        vec!['a', 'b'],
        vec!['a', 'c', 'c', 'c', 'b', 'a'],
    ];
    let mut random_loops = Vec::new();
    for (i, n) in [2usize, 0, 4].into_iter().enumerate() {
        let word = words.get(i).unwrap();
        random_loops.push(Reduced::new_omega(word, InfiniteLength(word.len(), n)))
    }
    (random_automata, words, random_loops)
}

fn finite_run_words((automata, words): (&[MooreMachine<Simple, usize>], &[Vec<char>])) {
    for automaton in automata {
        for word in words {
            if automaton.run_from(word, automaton.initial()).is_ok() {}
        }
    }
}

fn iai_runs() {
    finite_run_words(iai::black_box((&DATA.0, &DATA.1)));
}

fn finite_run_words_new((automata, words): (&[MooreMachine<Simple, usize>], &[Vec<char>])) {
    for automaton in automata {
        for word in words {
            if automaton.finite_run_from(automaton.initial(), word).is_ok() {}
        }
    }
}

fn iai_runs_new() {
    finite_run_words_new(iai::black_box((&DATA.0, &DATA.1)));
}

fn scc_tree_decomposition(automata: &[MooreMachine<Simple, usize>]) {
    for automaton in automata {
        automaton.tarjan_dag();
    }
}

fn iai_scc_tree_decomposition() {
    scc_tree_decomposition(iai::black_box(&DATA.0));
}

fn scc_decomposition(automata: &[MooreMachine<Simple, usize>]) {
    for automaton in automata {
        automaton.sccs();
    }
}

fn iai_scc_decomposition() {
    scc_decomposition(iai::black_box(&DATA.0))
}

fn automata_transformations_old(automata: &[MooreMachine<Simple, usize>]) {
    for automaton in automata {
        let _ts: automata::ts::BTS<_, _, _> = automaton
            .map_edge_colors(|_| 7)
            .map_state_colors(|_| 'c')
            .collect_old();
    }
}

fn iai_automata_transformations_old() {
    automata_transformations_old(iai::black_box(&DATA.0))
}

fn automata_transformations(automata: &[MooreMachine<Simple, usize>]) {
    for automaton in automata {
        let _ts: automata::ts::BTS<_, _, _> = automaton
            .map_edge_colors(|_| 7)
            .map_state_colors(|_| 'c')
            .collect();
    }
}

fn iai_automata_transformations() {
    automata_transformations(iai::black_box(&DATA.0))
}

fn simple_dfa_ops<A: Alphabet<Symbol = char, Expression = char>>(mut dfa: DFA<A>) {
    let q0 = dfa.initial();
    let q1 = dfa.add_state(false);
    let q2 = dfa.add_state(true);
    dfa.add_edge(q0, 'a', q0, ());
    dfa.add_edge(q0, 'b', q1, ());
    dfa.add_edge(q0, 'c', q2, ());
    dfa.add_edge(q1, 'a', q0, ());
    dfa.add_edge(q1, 'b', q1, ());
    dfa.add_edge(q1, 'c', q2, ());
    dfa.add_edge(q2, 'a', q0, ());
    dfa.add_edge(q2, 'b', q1, ());
    dfa.add_edge(q2, 'c', q2, ());
    for x in [
        "abbac",
        "abcbc",
        "abbcabbcbabcbbcbbabcbac",
        "bbcbabcbacbabcbabcbabcbabacbcbcbababbc",
    ] {
        let ReachedState(q) = dfa.induced(&x, dfa.initial()).unwrap();
        assert_eq!(q, 2);
    }
}

fn iai_simple_alphabet_ops() {
    let dfa = DFA::new(alphabet!(simple 'a', 'b', 'c'));
    simple_dfa_ops(iai::black_box(dfa));
}

fn iai_fixed_alphabet_ops() {
    let dfa = DFA::new(Fixed::from(['a', 'b', 'c']));
    simple_dfa_ops(iai::black_box(dfa));
}

iai::main!(
    iai_runs,
    iai_runs_new,
    iai_scc_decomposition,
    iai_scc_tree_decomposition,
    iai_automata_transformations_old,
    iai_automata_transformations,
    iai_simple_alphabet_ops,
    iai_fixed_alphabet_ops
);
