use automata::{alphabet::Simple, congurence::FORC};
use automata_learning::passive::OmegaSample;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use pprof::criterion::{Output, PProfProfiler};

fn build_sample() -> OmegaSample<Simple, bool> {
    let sample_str = r#"omega
    alphabet: a,b
    positive:
    bbabab
    ab
    baa
    abbab
    babab
    babba
    bbaba
    babab
    babba
    aba
    aab
    ababb
    a
    abab
    baba
    ba
    bbaba
    abbab
    babbba
    negative:
    bba
    abba
    baab
    bbba
    abb
    abbba
    bab
    bba
    babb
    bbab
    b
    bb
    abba
    bbaab
    abbb
    bbaa
    abbaa
    babbab
    bbabba
    babbb
    bbabb"#;
    OmegaSample::try_from(sample_str).unwrap()
}

fn infer_forc(sample: &OmegaSample<Simple, bool>) -> FORC<Simple> {
    sample.infer_forc()
}

fn criterion_benchmark(c: &mut Criterion) {
    let sample = build_sample();
    c.bench_function("forc_paper", |b| b.iter(|| infer_forc(black_box(&sample))));
}

criterion_group! {
    name = benches;
    config = Criterion::default().with_profiler(PProfProfiler::new(100, Output::Flamegraph(None)));
    targets = criterion_benchmark
}
criterion_main!(benches);
