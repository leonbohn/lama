use automata::alphabet::Simple;
use automata::word::Normalized;
use automata::{simple, ts::Sproutable};
use automata::{InfiniteLength, MooreMachine, Pointed, TransitionSystem};
use criterion::{black_box, criterion_group, criterion_main, Criterion};

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

const BENCH_SIZE: usize = 101;

fn random_automata(rand: &[usize]) -> Vec<MooreMachine<Simple, usize>> {
    let mut automata = vec![];
    for n in rand {
        automata.push(pseudorandom_sprout(BENCH_SIZE, *n));
    }
    automata
}

fn random_words() -> Vec<Vec<char>> {
    let mut words = Vec::with_capacity(BENCH_SIZE);
    for i in &RANDOM {
        let mut n = i % 4;
        let mut word = Vec::with_capacity(100);
        for _ in 0..(i % BENCH_SIZE) {
            word.push(['a', 'b', 'c', 'd'][n]);
            n = n.wrapping_add(*i) % 4;
        }
        words.push(word);
    }
    words
}

const RANDOM: [usize; 101] = [
    907898, 628828, 656295, 585375, 168920, 976400, 283751, 146218, 924485, 485174, 551114, 414085,
    23398, 692116, 364224, 527697, 329465, 180859, 197989, 675687, 986737, 697541, 2838, 341621,
    376278, 981507, 691163, 350350, 974365, 976987, 958451, 608452, 624096, 114510, 463275, 406791,
    877022, 125325, 820858, 493857, 835533, 311687, 33494, 188883, 816449, 373667, 399281, 797929,
    771335, 642879, 95832, 331000, 734027, 964638, 470433, 843028, 324676, 644087, 942716, 228245,
    480167, 637038, 822758, 827323, 190392, 607448, 541022, 250185, 35246, 266351, 323862, 30555,
    691076, 148060, 200864, 387120, 887100, 112734, 970894, 548249, 898204, 833388, 661808, 304235,
    233794, 405895, 324820, 69682, 455210, 860162, 408786, 51135, 944212, 639854, 261334, 624493,
    663173, 525156, 295525, 775720, 373995,
];

fn bench_sprout(c: &mut Criterion) {
    c.bench_function("random_sprout 100", |b| {
        b.iter(|| random_automata(black_box(&RANDOM)))
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

    let random_automata = random_automata(&RANDOM);
    let random_words = random_words();
    let mut random_loops = Vec::new();
    for (i, n) in RANDOM.iter().enumerate() {
        let word = &random_words[*n % BENCH_SIZE];
        let j = std::cmp::min(
            n.wrapping_mul(i + 3847) % word.len(),
            word.len().saturating_sub(1),
        );
        random_loops.push(Normalized::new_omega(word, InfiniteLength(word.len(), j)))
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
