use automata::prelude::*;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use itertools::Itertools;

type Automata = Vec<MooreMachine<Simple, usize>>;
type Words = Vec<Vec<char>>;
type Loops = Vec<Reduced<char>>;
lazy_static::lazy_static! {
static ref DATA: (
    Automata,
    Words,
    Loops,
    Vec<WithInitial<NTS<Simple, (), ()>>>
    ) = data();
}
const BENCH_SIZE: usize = 3;
const RANDOM: [usize; 3] = [3, 2, 7];

#[allow(clippy::type_complexity)]
fn data() -> (
    Vec<MooreMachine<Simple, usize>>,
    Vec<Vec<char>>,
    Vec<Reduced<char>>,
    Vec<WithInitial<NTS<Simple, (), ()>>>,
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
        random_loops.push(Reduced::ultimately_periodic(&word[..n], &word[n..]));
    }
    let random_nts = random_nts(10);
    (random_automata, words, random_loops, random_nts)
}
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

fn random_dts(
    n: usize,
) -> (
    WithInitial<BTS<Simple, (), ()>>,
    WithInitial<NTS<Simple, (), ()>>,
) {
    let mut ts = BTS::new(Simple::from_iter(['a', 'b']));
    for _ in 0..n {
        ts.add_state(());
    }
    for i in 0..n {
        ts.add_edge(i, 'a', (i + 1) % n, ());
        ts.add_edge(i, 'b', (i + 2) % n, ());
    }
    let dts = ts.with_initial(0);

    let it = (0..n).flat_map(|i| [(i, 'a', (), (i + 1) % n), (i, 'b', (), (i + 2) % n)]);
    let nts = NTS::builder().default_color(()).extend(it).with_initial(0);

    (dts, nts)
}

fn random_nts(n: usize) -> Vec<WithInitial<NTS<Simple, (), ()>>> {
    let it = (0..50).flat_map(|i| {
        [
            (i, 'a', (), (i + 17) * n % 50),
            (i, 'a', (), (i + 13) * n % 50),
            (i, 'b', (), i * n * n % 50),
        ]
    });
    vec![NTS::builder().default_color(()).extend(it).with_initial(0)]
}

fn scc_decomposition(aut: &[MooreMachine<Simple, usize>]) {
    for automaton in aut {
        automaton.tarjan_dag();
    }
}

fn predecessor_computation(aut: &[MooreMachine<Simple, usize>]) {
    for automaton in aut {
        for s in automaton.state_indices() {
            automaton.predecessors(s).unwrap().collect_vec();
        }
    }
}

fn bts_predecessors(bts: WithInitial<BTS<Simple, (), ()>>) {
    for x in bts.state_indices() {
        let _ = bts.predecessors(x).unwrap().count();
    }
}

fn nts_predecessors(bts: WithInitial<NTS<Simple, (), ()>>) {
    for x in bts.state_indices() {
        let _ = bts.predecessors(x).unwrap().count();
    }
}

fn powerset(aut: &[WithInitial<NTS<Simple, (), ()>>]) {
    for a in aut {
        let det = a.subset_construction();
        let _ = det.size();
        // assert!(det.size() >= a.size());
    }
}

fn bts_reachable(dts: WithInitial<BTS<Simple, (), ()>>) {
    let x = dts.reachable_state_indices().count();
    let _ = x % 3;
}
fn nts_reachable(nts: WithInitial<NTS<Simple, (), ()>>) {
    let y = nts.reachable_state_indices().count();
    let _ = y % 2;
}

fn benchings(c: &mut Criterion) {
    const STATES: usize = 2000;
    c.bench_function("BTS reachable states", |b| {
        b.iter(|| bts_reachable(random_dts(STATES).0))
    });
    c.bench_function("NTS reachable states", |b| {
        b.iter(|| nts_reachable(random_dts(STATES).1))
    });
    c.bench_function("BTS predecessors", |b| {
        b.iter(|| bts_predecessors(random_dts(STATES).0))
    });
    c.bench_function("NTS predecessors", |b| {
        b.iter(|| nts_predecessors(random_dts(STATES).1))
    });

    c.bench_function("tarjan_tree", |b| {
        b.iter(|| scc_decomposition(black_box(&DATA.0)))
    });
    c.bench_function("Predecessors", |b| {
        b.iter(|| predecessor_computation(black_box(&DATA.0)))
    });
    c.bench_function("Subset construction", |b| {
        b.iter(|| powerset(black_box(&DATA.3)))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = benchings
}
criterion_main!(benches);
