use automata::{
    prelude::*,
    ts::{dot::MightDecorateDotTransition, IndexedAlphabet},
};
use owo_colors::OwoColorize;
use tracing::trace;

/// Contains definitions for samples, which are collections of positive and
/// negative example words.
#[macro_use]
pub mod sample;
pub use sample::{ClassOmegaSample, PeriodicOmegaSample, Sample, SplitOmegaSample};

use crate::{
    active::{LStar, MealyOracle},
    passive::fwpm::FWPM,
    prefixtree::prefix_tree,
    AnnotatedCongruence,
};

use self::precise::PreciseDPA;

pub use self::sample::{FiniteSample, OmegaSample};

/// Module containing the implementations of the sprout/glerc algorithm.
pub mod sprout;

/// Deals with families of weak priority mappings.
pub mod fwpm;

/// Defines the precise DPA.
pub mod precise;

/// Executes the RPNI algorithm on the given sample. This returns a DFA that is
/// composed of a right congruence as well as an acceptance condition, which marks
/// a classes as accepting if it is reached by a positive sample word.
pub fn dfa_rpni<A: Alphabet>(sample: &FiniteSample<A, bool>) -> DFA<A> {
    let cong = sprout::sprout(sample, vec![], true);
    let accepting: automata::Set<_> = sample
        .positive_words()
        .map(|w| {
            cong.reached_state_index(w)
                .expect("Every positive word must induce a successful run!")
        })
        .collect();
    (&cong)
        .map_state_colors(|q| {
            let idx = cong
                .class_to_index(q.class())
                .expect("Class must be in the congruence!");
            accepting.contains(&idx)
        })
        .collect_with_initial()
}

/// Executes a variant of the RPNI algorithm for omega-words, producing a DBA.
pub fn dba_rpni<A: Alphabet>(sample: &OmegaSample<A, bool>) -> DBA<A> {
    todo!()
}

/// Takes a reference to an [`InfiniteSample`], which classifies infinite words over the alphabet `A`
/// with boolean values and infers a [`PreciseDPA`] from it. The steps for this are roughly
/// - infer the leading prefix (aka Myhill/Nerode) congruence
/// - produce a [`SplitOmegaSample`] such that each suffix of a sample word which "starts" in a class `c`
///   gets put into the respective component of the sample for class `c`
/// - uses the glerc/sprout algorithm to infer a family of right congruences (FORC)
/// - color the FORC to obtain a canonical coloring for each class; these are then combined to an FWPM using
///   the leading congruence infered before
/// - build the precise DPA from the FWPM
pub fn infer_precise_dpa<A: Alphabet>(
    sample: &OmegaSample<A, bool>,
) -> PreciseDPA<A, { precise::PRECISE_DPA_COLORS }> {
    let cong = sample.infer_right_congruence();
    let split = sample.split(&cong);

    let forc = split.infer_forc();
    trace!("{}\n{:?}", "INFERRED FORC".bold(), forc);

    let mut fwpm = FWPM::empty(&cong);
    for (class, idx) in cong.classes() {
        let periodic_sample = split.get(idx).expect("Must exist!").to_periodic_sample();
        let annotated_prc =
            AnnotatedCongruence::build(forc.prc(idx).expect("Must exist"), &periodic_sample);
        trace!(
            "{} for class {:?}\t{:?}",
            "ANNOTATED CONGRUENCE".bold().blue(),
            class,
            annotated_prc
        );
        let coloring = annotated_prc.canonic_coloring();
        trace!("{}{:?}", "inferred ".green(), coloring);
        fwpm.insert_pm(idx, coloring);
    }
    trace!("Calculated the FWPM\n{:?}", fwpm);
    fwpm.into_precise_dpa()
}

/// Similar to [`dba_rpni`], but produces a DPA instead.
pub fn dpa_rpni<A: Alphabet>(sample: &OmegaSample<A, bool>) -> DPA<A, (), MealyMachine<A>>
where
    A: IndexedAlphabet,
{
    let precise = infer_precise_dpa(sample);
    let pta = sample.prefix_tree().erase_state_colors();

    let prod = precise
        .ts_product(pta)
        .map_edge_colors(|(c, _)| c)
        .map_state_colors(|(_, _)| ());
    let completed = prod.collect_complete_with_initial((), 0);

    //now we use the completed thing to learn a MealyMachine from which we can then build the DPA
    let mm = completed.into_mealy();
    let alphabet = mm.alphabet().clone();
    let oracle = MealyOracle::new(mm);

    let learned = LStar::mealy_unlogged(oracle, alphabet).infer();
    learned.into_dpa()
}

#[cfg(test)]
mod tests {
    use automata::prelude::*;
    use tracing_test::traced_test;

    use crate::passive::dpa_rpni;

    use super::{sample, OmegaSample};

    #[test]
    fn infer_precise_dpa_inf_aa() {
        let alphabet = alphabet!(simple 'a', 'b', 'c');
        let sample = sample! {alphabet; pos "a", "aab", "aaab", "bbaa", "aca", "caa", "abca", "baac"; neg "c", "b", "bc", "abc", "cba", "ac", "ba"};

        let t = std::time::Instant::now();
        let dpa = super::infer_precise_dpa(&sample).into_dpa();
        let full_duration = t.elapsed().as_millis();
        println!(
            "{}",
            dpa.build_transition_table(|q, c| format!("{}|{:?}", q, c))
        );

        let expected = [
            (upw!("a"), true),
            (upw!("baa"), true),
            (upw!("cabaca"), false),
            (upw!("baacbacbac"), true),
        ];
        for (w, c) in &expected {
            let b = dpa.accepts_omega(&w);
            assert_eq!(b, *c, "{:?} is classified {b}, expected {c}", w);
        }

        let t = std::time::Instant::now();
        let dpa = dpa_rpni(&sample);
        let paper_duration = t.elapsed().as_millis();

        println!(
            "Full construction took {full_duration}ms, paper construction took {paper_duration}ms"
        );

        for (w, c) in expected {
            let b = dpa.accepts_omega(&w);
            assert_eq!(b, c, "{:?} is classified {b}, expected {c}", w);
        }
    }
}
