use automata::{prelude::*, ts::dot::MightDecorateDotTransition};
use owo_colors::OwoColorize;
use tracing::trace;

/// Contains definitions for samples, which are collections of positive and
/// negative example words.
#[macro_use]
pub mod sample;
pub use sample::{ClassOmegaSample, PeriodicOmegaSample, Sample, SplitOmegaSample};

use crate::{passive::fwpm::FWPM, AnnotatedCongruence};

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
pub fn dpa_rpni<A: Alphabet>(sample: &OmegaSample<A, bool>) -> DPA<A> {
    todo!()
}

#[cfg(test)]
mod tests {
    use automata::prelude::*;
    use tracing_test::traced_test;

    use super::{sample, OmegaSample};

    #[test]
    #[traced_test]
    fn infer_precise_dpa_inf_aa() {
        let alphabet = alphabet!(simple 'a', 'b', 'c');
        let sample = sample! {alphabet; pos "a", "aab", "aaab", "bbaa", "aca", "caa", "abca", "baac"; neg "c", "b", "bc", "abc", "cba", "ac", "ba"};
        let dpa = super::infer_precise_dpa(&sample);
        println!("{:?}", dpa);
        let dpa = dpa.into_dpa();

        for (w, c) in [
            (upw!("a"), true),
            (upw!("baa"), true),
            (upw!("cabaca"), false),
            (upw!("baacbacbac"), true),
        ] {
            let b = dpa.accepts_omega(&w);
            assert_eq!(b, c, "{:?} is classified {b}, expected {c}", w);
        }
    }
}
