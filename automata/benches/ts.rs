use automata::alphabet::Simple;
use automata::word::Normalized;
use automata::{simple, ts::Sproutable};
use automata::{InfiniteLength, MooreMachine, Pointed, TransitionSystem};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

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

fn bench_sprout(c: &mut Criterion) {
    c.bench_function("random_sprout", |b| {
        b.iter(|| random_automata(black_box(&RANDOM)))
    });
}

fn finite_run_words((automata, words): (&[MooreMachine<Simple, usize>], &[Vec<char>])) {
    for automaton in automata {
        for word in words {
            if automaton.run(word, automaton.initial()).is_ok() {}
        }
    }
}
fn finite_run_words_new((automata, words): (&[MooreMachine<Simple, usize>], &[Vec<char>])) {
    for automaton in automata {
        for word in words {
            if automaton.finite_run(automaton.initial(), word).is_ok() {}
        }
    }
}

#[allow(clippy::type_complexity)]
fn infinite_run_words(
    (automata, words): (
        &[MooreMachine<Simple, usize>],
        &[Normalized<char, InfiniteLength>],
    ),
) {
    for automaton in automata {
        for word in words {
            if automaton.run(word, automaton.initial()).is_ok() {}
        }
    }
}
#[allow(clippy::type_complexity)]
fn infinite_run_words_new(
    (automata, words): (
        &[MooreMachine<Simple, usize>],
        &[Normalized<char, InfiniteLength>],
    ),
) {
    for automaton in automata {
        for w in words {
            if automaton
                .omega_run(
                    automaton.initial(),
                    w.initial_segment(),
                    w.repeating_segment(),
                )
                .is_ok()
            {}
        }
    }
}

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

fn bench_run(c: &mut Criterion) {
    let (random_automata, random_words, random_loops) = data();

    c.bench_function("finite_random_runs 100", |b| {
        b.iter(|| finite_run_words(black_box((&random_automata, &random_words))))
    });
    c.bench_function("finite_random_runs_new 100", |b| {
        b.iter(|| finite_run_words_new(black_box((&random_automata, &random_words))))
    });
    c.bench_function("infinite_random_runs 100", |b| {
        b.iter(|| infinite_run_words(black_box((&random_automata, &random_loops))))
    });
    c.bench_function("infinite_random_runs_new 100", |b| {
        b.iter(|| infinite_run_words_new(black_box((&random_automata, &random_loops))))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = bench_sprout, bench_run
}
criterion_main!(benches);
