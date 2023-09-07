use automata::alphabet::Simple;
use automata::ts::FiniteState;
use automata::word::Normalized;
use automata::{simple, ts::Sproutable};
use automata::{InfiniteLength, MooreMachine, TransitionSystem};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use itertools::Itertools;

type Automata = Vec<MooreMachine<Simple, usize>>;
type Words = Vec<Vec<char>>;
type Loops = Vec<Normalized<char, InfiniteLength>>;
lazy_static::lazy_static! {
static ref DATA: (
    Automata,
    Words,
    Loops,
    ) = data();
}
const BENCH_SIZE: usize = 3;
const RANDOM: [usize; 3] = [3, 2, 7];

#[allow(clippy::type_complexity)]
fn data() -> (
    Vec<MooreMachine<Simple, usize>>,
    Vec<Vec<char>>,
    Vec<Normalized<char, InfiniteLength>>,
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
        random_loops.push(Normalized::new_omega(word, InfiniteLength(word.len(), n)))
    }
    (random_automata, words, random_loops)
}
fn pseudorandom_sprout(states: usize, n: usize) -> MooreMachine<Simple, usize> {
    let alphabet = simple!('a', 'b', 'c', 'd');

    let mut ts = automata::MooreMachine::new(alphabet);

    for i in 0..states {
        ts.add_state(i);
    }

    let mut c: usize = 0;
    for sym in ['a', 'b', 'c', 'd'] {
        for i in 0..states {
            ts.add_edge(i, sym, c, ());
            c = c.wrapping_add(n) % BENCH_SIZE;
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

fn scc_decomposition(aut: &[MooreMachine<Simple, usize>]) {
    for automaton in aut {
        automaton.tarjan_tree();
    }
}

fn predecessor_computation(aut: &[MooreMachine<Simple, usize>]) {
    for automaton in aut {
        for s in automaton.state_indices() {
            automaton.predecessors(s).unwrap().collect_vec();
        }
    }
}

fn benchings(c: &mut Criterion) {
    c.bench_function("tarjan_tree", |b| {
        b.iter(|| scc_decomposition(black_box(&DATA.0)))
    });
    c.bench_function("Predecessors", |b| {
        b.iter(|| predecessor_computation(black_box(&DATA.0)))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = benchings
}
criterion_main!(benches);
