#![allow(clippy::type_complexity)]
use automata::prelude::*;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use itertools::Itertools;

fn bench_dts() -> Initialized<DTS<CharAlphabet, usize, usize>> {
    TSBuilder::default()
        .with_colors([1, 13, 72, 891, 3, 5])
        .with_transitions([
            (0, 'a', 2, 0),
            (0, 'b', 65, 1),
            (0, 'c', 9, 4),
            (0, 'd', 8, 3),
            (1, 'a', 12, 5),
            (1, 'b', 165, 4),
            (1, 'c', 19, 2),
            (1, 'd', 18, 5),
            (2, 'a', 12, 0),
            (2, 'b', 165, 1),
            (2, 'c', 19, 3),
            (2, 'd', 18, 2),
            (3, 'a', 12, 5),
            (3, 'b', 165, 2),
            (3, 'c', 19, 3),
            (3, 'd', 18, 0),
            (4, 'a', 12, 5),
            (4, 'b', 165, 4),
            (4, 'c', 19, 5),
            (4, 'd', 18, 2),
            (5, 'a', 12, 0),
            (5, 'b', 165, 1),
            (5, 'c', 19, 3),
            (5, 'd', 18, 5),
        ])
        .deterministic()
        .with_initial(0)
}

fn benchings(c: &mut Criterion) {
    let dts = bench_dts();
    let ref_dts = &dts;

    c.bench_function("DTS reachable states", |b| {
        b.iter(|| black_box(dts.reachable_state_indices().collect_vec()))
    });
    c.bench_function("DTS predecessors", |b| {
        b.iter(|| {
            for q in dts.state_indices() {
                black_box(dts.predecessors(q).unwrap().collect_vec());
            }
        })
    });
    c.bench_function("tarjan_tree", |b| b.iter(|| black_box(dts.tarjan_dag())));
    c.bench_function("transform intersperse collect", |b| {
        b.iter(|| {
            black_box(
                ref_dts
                    .map_edge_colors(|i| i * 2 - 7)
                    .collect_dts()
                    .map_state_colors(|j| j * 7 - 4)
                    .collect_dts(),
            );
        })
    });
    c.bench_function("transform then collect", |b| {
        b.iter(|| {
            black_box(
                ref_dts
                    .map_edge_colors(|i| i + 2 - 7)
                    .map_state_colors(|j| j * 7 - 4)
                    .collect_dts(),
            )
        })
    });
    c.bench_function("finite runs", |b| {
        b.iter(|| {
            for word in [
                "abbabbabaccbab",
                "",
                "abcbabadbadbacdc",
                "cccbabcbacbdbaccbcbd",
                "ddacacdbadcdbacbadcb",
                "dddddddddddddddddddddddddddddddddddd",
            ] {
                black_box(dts.reached_state_index(word).unwrap());
            }
        })
    });
    c.bench_function("omega runs", |b| {
        b.iter(|| {
            for word in [
                upw!("abba", "cabad"),
                upw!("cddcadcabdab"),
                upw!("acadbabd"),
                upw!("badbdabbdcadb", "adbdadcadcaddacadcbadbcabbdcbadcbabdc"),
            ] {
                black_box(dts.omega_run(word).unwrap());
            }
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = benchings
}
criterion_main!(benches);
