use automata::{alphabet::Simple, congruence::FORC};
use automata_learning::passive::InfiniteSample;

fn build_sample() -> InfiniteSample<Simple, bool> {
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
    InfiniteSample::try_from(sample_str).unwrap()
}

fn infer_forc(sample: &InfiniteSample<Simple, bool>) -> FORC<Simple> {
    let cong = sample.infer_right_congruence();
    let split = sample.split(&cong);
    split.infer_forc()
}

fn iai_infer_forc() {
    let sample = build_sample();
    infer_forc(iai::black_box(&sample));
}

iai::main!(iai_infer_forc);
