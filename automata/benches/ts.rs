use std::time::Duration;

use automata::alphabet::Simple;
use automata::word::Normalized;
use automata::{simple, ts::Sproutable};
use automata::{InfiniteLength, MooreMachine, Pointed, Successor};
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use pprof::criterion::{Output, PProfProfiler};
use rand::rngs::SmallRng;
use rand::{thread_rng, Rng, SeedableRng};

fn pseudorandom_sprout(states: usize, b: usize, r: usize) -> MooreMachine<Simple, usize> {
    let alphabet = simple!('a', 'b', 'c', 'd');

    let mut ts = automata::MooreMachine::new(alphabet);

    for i in 0..states {
        let state = ts.add_state(i);
    }

    let mut c = b;
    for sym in ['a', 'b', 'c', 'd'] {
        for i in 0..states {
            ts.add_edge(i, sym, c % states, ());
            c = (b.wrapping_mul(c.wrapping_add(r))).wrapping_add(r);
        }
    }
    ts
}

const BENCH_SIZE: usize = 50;
const B_RANGE: (usize, usize) = (1, 343);
const R_RANGE: (usize, usize) = (664, 234873);

fn give_rand() -> Vec<(usize, usize)> {
    let mut rng = thread_rng();
    let mut values = Vec::with_capacity(BENCH_SIZE);
    for _ in 0..BENCH_SIZE {
        values.push((
            rng.gen_range(B_RANGE.0..B_RANGE.1),
            rng.gen_range(R_RANGE.0..R_RANGE.1),
        ));
    }
    values
}

fn random_automata(rand: &[(usize, usize)]) -> Vec<MooreMachine<Simple, usize>> {
    let mut automata = vec![];
    for (b, r) in rand {
        automata.push(pseudorandom_sprout(BENCH_SIZE, *b, *r));
    }
    automata
}

fn random_words() -> Vec<Vec<char>> {
    let mut rng = thread_rng();
    let mut words = Vec::with_capacity(BENCH_SIZE);
    for _ in 0..BENCH_SIZE {
        let mut word = Vec::with_capacity(100);
        for _ in 0..rng.gen_range(2..BENCH_SIZE) {
            word.push(rng.gen_range('a'..='d'));
        }
        words.push(word);
    }
    words
}

fn bench_sprout(c: &mut Criterion) {
    let random = give_rand();

    c.bench_function("random_sprout 100", |b| {
        b.iter(|| random_automata(black_box(&random)))
    });
}

fn bench_run(c: &mut Criterion) {
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

    let random_automata = random_automata(&give_rand());
    let random_words = random_words();
    let mut random_loops = Vec::new();
    for word in &random_words {
        let i = thread_rng().gen_range(0..(word.len().saturating_sub(1)));
        random_loops.push(Normalized::new_omega(word, InfiniteLength(word.len(), i)))
    }

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
