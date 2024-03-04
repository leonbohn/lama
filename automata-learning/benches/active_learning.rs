use automata::prelude::*;
use automata_learning::{
    active::{LStar, SampleOracle},
    passive::FiniteSample,
};

fn build_sample_oracle_last_sym() -> SampleOracle<CharAlphabet, Vec<char>, usize> {
    let alphabet = alphabet!(simple 'a', 'b');
    let classified_words = [
        "a", "b", "aa", "ab", "ba", "bb", "aaa", "aab", "aba", "abb", "baa", "bab", "bba", "bbb",
    ]
    .into_iter()
    .map(|w| (w.chars(), if w.ends_with('a') { 0 } else { 1 }));
    let sample = FiniteSample::new_finite(alphabet.clone(), classified_words);
    SampleOracle::new(sample, 0)
}

fn lstar_last_sym(
    sample: SampleOracle<CharAlphabet, Vec<char>, usize>,
) -> MooreMachine<CharAlphabet, usize> {
    let alphabet = sample.alphabet().clone();
    let mut lstar = LStar::for_moore(alphabet, sample);
    lstar.infer()
}

fn iai_lstar_last_sym() {
    let sample_oracle = build_sample_oracle_last_sym();
    lstar_last_sym(iai::black_box(sample_oracle));
}

fn lstar_last_sym_mealy(
    sample: SampleOracle<CharAlphabet, Vec<char>, usize>,
) -> MealyMachine<CharAlphabet, usize> {
    let alphabet = sample.alphabet().clone();
    let mut lstar = LStar::for_mealy(alphabet, sample);
    lstar.infer()
}

fn iai_lstar_last_sym_mealy() {
    let sample_oracle = build_sample_oracle_last_sym();
    lstar_last_sym_mealy(iai::black_box(sample_oracle));
}

iai::main!(iai_lstar_last_sym, iai_lstar_last_sym_mealy);
