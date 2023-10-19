use std::fmt::Debug;

use automata::prelude::*;
use itertools::Itertools;
use tracing::trace;

use super::oracle::Oracle;

const ITERATION_THRESHOLD: usize = 2000;

pub type LStarExample<A, Q> = (Vec<<A as Alphabet>::Symbol>, Q);

#[derive(Clone)]
pub enum LStarQuery<A: Alphabet, Q: Color> {
    Membership(LStarExample<A, Q>),
    Equivalence(MooreMachine<A, Q>, Option<LStarExample<A, Q>>),
}

impl<A: Alphabet, Q: Color + Debug> std::fmt::Debug for LStarQuery<A, Q> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Membership((w, c)) => write!(f, "Membership({:?}) = {:?}", w, c),
            Self::Equivalence(hyp, cex) => match cex {
                Some((w, c)) => write!(f, "Equivalence({:?}, {:?}) = {:?}", hyp, w, c),
                None => write!(f, "Consistent hypothesis {:?}", hyp),
            },
        }
    }
}

impl<A: Alphabet, Q: Color> LStarQuery<A, Q> {
    pub fn is_equivalence(&self) -> bool {
        matches!(self, LStarQuery::Equivalence(_, _))
    }

    pub fn is_successful_equivalence(&self) -> bool {
        matches!(self, LStarQuery::Equivalence(_, None))
    }

    pub fn example(&self) -> Option<LStarExample<A, Q>> {
        match self {
            LStarQuery::Membership(ex) => Some(ex.clone()),
            LStarQuery::Equivalence(_, Some(ex)) => Some(ex.clone()),
            _ => None,
        }
    }
}

pub trait LStarLogger<A: Alphabet, Q: Color> {
    fn log(&mut self, query: LStarQuery<A, Q>);
    fn create() -> Self;
}

impl<A: Alphabet, Q: Color> LStarLogger<A, Q> for () {
    fn log(&mut self, query: LStarQuery<A, Q>) {}

    fn create() -> Self {}
}

#[derive(Debug, Clone)]
pub struct LStarLogbook<A: Alphabet, Q: Color>(Vec<LStarQuery<A, Q>>);

impl<A: Alphabet, Q: Color> LStarLogbook<A, Q> {
    pub fn is_sane(&self) -> bool {
        match self
            .0
            .iter()
            .map(|e| match e {
                LStarQuery::Membership(_) => 0,
                LStarQuery::Equivalence(_, x) => {
                    if x.is_some() {
                        0
                    } else {
                        1
                    }
                }
            })
            .reduce(|acc, x| acc + x)
            .unwrap_or(0)
        {
            0 => true,
            1 => self.0.last().unwrap().is_successful_equivalence(),
            _ => unreachable!("This should never be possible"),
        }
    }

    pub fn examples(&self) -> impl Iterator<Item = LStarExample<A, Q>> + '_ {
        assert!(self.is_sane());
        self.0.iter().filter_map(|e| e.example()).unique()
    }
}

impl<A: Alphabet, Q: Color> LStarLogger<A, Q> for LStarLogbook<A, Q> {
    fn log(&mut self, query: LStarQuery<A, Q>) {
        assert!(self.is_sane());
        self.0.push(query);
    }

    fn create() -> Self {
        Self(vec![])
    }
}

#[derive(Debug, Clone)]
pub struct LStarRow<S: Symbol, C: Color> {
    base: Vec<S>,
    minimal: bool,
    outputs: Vec<C>,
}

impl<S: Symbol, C: Color> PartialEq for LStarRow<S, C> {
    fn eq(&self, other: &Self) -> bool {
        self.outputs == other.outputs
    }
}

impl<S: Symbol, C: Color> LStarRow<S, C> {
    pub fn new(base: Vec<S>, outputs: Vec<C>) -> Self {
        Self {
            minimal: base.is_empty(),
            base,
            outputs,
        }
    }

    pub fn get(&self, index: usize) -> Option<&C> {
        self.outputs.get(index)
    }
}

#[derive(Debug, Clone)]
pub struct LStar<
    A: Alphabet,
    Q: Color,
    T: Oracle<Alphabet = A, Output = Q>,
    L: LStarLogger<A, Q> = LStarLogbook<A, Q>,
> {
    teacher: T,
    alphabet: A,
    experiments: Vec<Vec<A::Symbol>>,
    rows: Vec<LStarRow<A::Symbol, Q>>,
    logger: L,
}

impl<
        A: Alphabet,
        Q: Color + Default + std::fmt::Debug,
        T: Oracle<Length = FiniteLength, Alphabet = A, Output = Q>,
    > LStar<A, Q, T, ()>
{
    pub fn unlogged(teacher: T, alphabet: A) -> LStar<A, Q, T, ()> {
        LStar::new(teacher, alphabet)
    }
}

impl<
        A: Alphabet,
        Q: Color + Default + std::fmt::Debug,
        T: Oracle<Length = FiniteLength, Alphabet = A, Output = Q>,
    > LStar<A, Q, T, LStarLogbook<A, Q>>
{
    pub fn logged(teacher: T, alphabet: A) -> LStar<A, Q, T, LStarLogbook<A, Q>> {
        LStar::new(teacher, alphabet)
    }
}

impl<
        A: Alphabet,
        Q: Color + Default + std::fmt::Debug,
        T: Oracle<Length = FiniteLength, Alphabet = A, Output = Q>,
        L: LStarLogger<A, Q>,
    > LStar<A, Q, T, L>
{
    pub fn new(teacher: T, alphabet: A) -> Self {
        let experiments = std::iter::once(vec![])
            .chain(alphabet.universe().map(|sym| vec![*sym]))
            .collect_vec();

        let mut logger = L::create();
        let rows = vec![LStarRow::new(
            vec![],
            experiments
                .iter()
                .map(|experiment| {
                    let output = teacher.output(experiment);
                    logger.log(LStarQuery::Membership((
                        experiment.to_owned(),
                        output.clone(),
                    )));
                    output
                })
                .collect_vec(),
        )];

        LStar {
            teacher,
            alphabet,
            experiments,
            rows,
            logger: L::create(),
        }
    }

    pub fn logs(&self) -> &L {
        &self.logger
    }

    pub fn infer(&mut self) -> MooreMachine<A, Q> {
        let mut iteration = 0;
        loop {
            if iteration > ITERATION_THRESHOLD {
                panic!("Too many iterations");
            }
            iteration += 1;
            trace!("Lstar iteration {iteration}");
            let hypothesis = self.hypothesis();
            match self.teacher.equivalence(&hypothesis) {
                Ok(_) => return hypothesis,
                Err(conflict) => {
                    // trace!(
                    //     "Obtained counterexample \"{}\" with classification {:?}",
                    //     conflict.0.iter().map(|sym| format!("{:?}", sym)).join(""),
                    //     conflict.1
                    // );
                    self.process_counterexample(hypothesis, conflict)
                }
            }
        }
    }

    pub fn process_counterexample(
        &mut self,
        hypothesis: MooreMachine<A, Q>,
        (counterexample, expected_color): LStarExample<A, Q>,
    ) {
        debug_assert!(
            hypothesis.transform(&counterexample) != expected_color,
            "This is not a real counterexample"
        );
        self.logger.log(LStarQuery::Equivalence(
            hypothesis.clone(),
            Some((counterexample.clone(), expected_color.clone())),
        ));

        let mut state = hypothesis.initial();
        let mut previous_color = expected_color;
        for i in 0..counterexample.len() {
            let sym = counterexample[i];
            let next_state = hypothesis
                .successor_index(state, sym)
                .expect("We assume the hypothesis to be deterministic!");
            let suffix = counterexample.offset(i + 1);
            let next_color = (&hypothesis).with_initial(next_state).transform(&suffix);

            if next_color != previous_color {
                // this is the breakpoint
                let experiment = suffix.finite_to_vec();
                debug_assert!(
                    !self.experiments.contains(&experiment),
                    "We assume that the counterexample is new!"
                );

                for row_index in 0..self.rows.len() {
                    let output = self
                        .teacher
                        .output((&self.rows[row_index].base).append(&experiment));
                }
                self.experiments.push(experiment);
                return;
            }
        }
        unreachable!("A breakpoint has to exist!");
    }

    pub fn hypothesis(&mut self) -> MooreMachine<A, Q>
    where
        Q: Default,
    {
        trace!(
            "Computing hypothesis with experiments {{{}}}",
            self.experiments
                .iter()
                .map(|experiment| experiment.iter().map(|sym| format!("{:?}", sym)).join(""))
                .join(", ")
        );
        'outer: loop {
            let mut out: MooreMachine<A, Q> =
                MooreMachine::new(self.alphabet.clone(), self.rows[0].outputs[0].clone());

            let state_mapping: Vec<_> = std::iter::once((vec![], out.initial()))
                .chain(self.rows.iter().skip(1).enumerate().filter_map(|(i, row)| {
                    if row.minimal {
                        Some((row.base.clone(), out.add_state(row.outputs[0].clone())))
                    } else {
                        None
                    }
                }))
                .collect();
            trace!(
                "Considering {} base states {{{}}}",
                state_mapping.len(),
                state_mapping
                    .iter()
                    .map(|(base, _)| format!(
                        "{} | {}",
                        base.iter().map(|sym| format!("{:?}", sym)).join(""),
                        self.rows
                            .iter()
                            .find_map(|row| if row.base == *base {
                                Some(
                                    row.outputs
                                        .iter()
                                        .map(|sym| format!("{:?}", sym))
                                        .join(", "),
                                )
                            } else {
                                None
                            })
                            .unwrap()
                    ))
                    .join(", ")
            );
            for i in 0..state_mapping.len() {
                let (base, state) = &state_mapping[i];
                'transition: for sym in self.alphabet.universe() {
                    let mut extension: Vec<_> =
                        base.iter().cloned().chain(std::iter::once(*sym)).collect();
                    let mut new_row = LStarRow::new(
                        extension.clone(),
                        self.experiments
                            .iter()
                            .map(|experiment| {
                                let word = (&extension).append(experiment);
                                let output = self.teacher.output(&word);
                                self.logger.log(LStarQuery::Membership((
                                    word.finite_to_vec(),
                                    output.clone(),
                                )));
                                output
                            })
                            .collect_vec(),
                    );

                    trace!(
                        "Searching equivalent row for {}|[{}]",
                        new_row.base.iter().map(|sym| format!("{:?}", sym)).join(""),
                        new_row
                            .outputs
                            .iter()
                            .map(|sym| format!("{:?}", sym))
                            .join(", ")
                    );
                    if let Some(equivalent_row) = self.rows.iter().find(|row| row == &&new_row) {
                        trace!(
                            "Found equivalent row {}|[{}]",
                            equivalent_row
                                .base
                                .iter()
                                .map(|sym| format!("{:?}", sym))
                                .join(""),
                            equivalent_row
                                .outputs
                                .iter()
                                .map(|sym| format!("{:?}", sym))
                                .join(", "),
                        );
                        out.add_edge(
                            *state,
                            A::expression(*sym),
                            state_mapping
                                .iter()
                                .find_map(|(base, idx)| {
                                    if base == &equivalent_row.base {
                                        Some(*idx)
                                    } else {
                                        None
                                    }
                                })
                                .unwrap(),
                            (),
                        );
                        continue 'transition;
                    } else {
                        trace!("No equivalent row exists, adding new state");
                        new_row.minimal = true;
                        self.rows.push(new_row);
                        continue 'outer;
                    }
                }
            }
            return out;
        }
    }
}

#[cfg(test)]
mod tests {
    use automata::prelude::*;
    use owo_colors::OwoColorize;
    use tracing_test::traced_test;

    use crate::active::{
        oracle::{self, DFAOracle},
        Oracle,
    };

    struct ModkAmodlB(Simple);
    struct WordLenModk(Simple, usize);

    impl HasAlphabet for ModkAmodlB {
        type Alphabet = Simple;

        fn alphabet(&self) -> &Self::Alphabet {
            &self.0
        }
    }

    impl Oracle for ModkAmodlB {
        type Output = usize;

        fn output<
            W: automata::Word<Symbol = automata::alphabet::SymbolOf<Self>, Length = Self::Length>,
        >(
            &self,
            word: W,
        ) -> Self::Output {
            let (count_a, count_b) =
                word.finite_to_vec()
                    .into_iter()
                    .fold((0, 0), |(a, b), c| match c {
                        'a' => (a + 1, b),
                        'b' => (a, b + 1),
                        _ => unreachable!(),
                    });

            if count_a % 2 == 0 && count_b % 2 == 0 {
                1
            } else {
                0
            }
        }

        fn equivalence<H>(
            &self,
            hypothesis: H,
        ) -> Result<(), (Vec<automata::alphabet::SymbolOf<Self>>, Self::Output)>
        where
            H: automata::ts::Pointed
                + automata::ts::TransitionSystem<Alphabet = Self::Alphabet, StateColor = Self::Output>,
        {
            for word in ["aa", "bb", "bab", "aba", "abba", "bbab", "", "b", "a"] {
                let output = self.output(&word);
                if output != hypothesis.transform(&word) {
                    return Err((word.chars().collect(), output));
                }
            }
            Ok(())
        }

        type Length = FiniteLength;
    }

    impl HasAlphabet for WordLenModk {
        type Alphabet = Simple;

        fn alphabet(&self) -> &Self::Alphabet {
            &self.0
        }
    }

    #[cfg(test)]
    impl Oracle for WordLenModk {
        type Output = usize;
        type Length = FiniteLength;

        fn output<
            W: automata::Word<Length = Self::Length, Symbol = automata::alphabet::SymbolOf<Self>>,
        >(
            &self,
            word: W,
        ) -> Self::Output {
            word.length().0 % self.1
        }

        fn equivalence<
            H: automata::ts::Pointed
                + automata::ts::TransitionSystem<Alphabet = Self::Alphabet, StateColor = Self::Output>,
        >(
            &self,
            hypothesis: H,
        ) -> Result<(), (Vec<automata::alphabet::SymbolOf<Self>>, Self::Output)> {
            for word in [
                "aa", "bb", "bab", "bbabba", "aba", "abba", "bbab", "", "b", "a",
            ] {
                let word = OmegaWord::new_reverse_args(FiniteLength(word.len()), word);
                let output = self.output(&word);
                if output != hypothesis.transform(&word) {
                    return Err((word.to_vec(), output));
                }
            }
            Ok(())
        }
    }

    #[test]
    fn lstar_word_len_mod_k() {
        let alphabet = Simple::from_iter(vec!['a', 'b']);

        for k in (30..=50) {
            let time_start = std::time::Instant::now();
            let oracle = WordLenModk(alphabet.clone(), k);
            let mut lstar = super::LStar::unlogged(oracle, alphabet.clone());
            let mm = lstar.infer();
            let time_taken = time_start.elapsed().as_micros();
            assert_eq!(mm.size(), k);
            println!("Took {:>6}μs for k={}", time_taken, k);
        }
    }

    #[test]
    #[traced_test]
    fn lstar_even_a_even_b() {
        let alphabet = Simple::from_iter(vec!['a', 'b']);
        let mut oracle = ModkAmodlB(alphabet.clone());
        let mut lstar = super::LStar::unlogged(oracle, alphabet);

        let mm = lstar.infer();

        assert_eq!(mm.transform("abba"), 1);
        assert_eq!(mm.transform("ab"), 0);
        assert_eq!(mm.transform(""), 1)
    }

    fn test_dfa() -> DFA {
        let alphabet = Simple::from_iter(['a', 'b', 'c']);
        let mut dfa = DFA::new(alphabet);
        let q0 = dfa.initial();
        dfa.set_initial_color(true);
        let q1 = dfa.add_state(false);
        let q2 = dfa.add_state(true);
        let q3 = dfa.add_state(false);
        dfa.add_edge(q0, 'a', q1, ());
        dfa.add_edge(q0, 'b', q3, ());
        dfa.add_edge(q0, 'c', q0, ());
        dfa.add_edge(q1, 'a', q0, ());
        dfa.add_edge(q1, 'b', q2, ());
        dfa.add_edge(q1, 'c', q0, ());
        dfa.add_edge(q2, 'a', q2, ());
        dfa.add_edge(q2, 'b', q2, ());
        dfa.add_edge(q2, 'c', q0, ());
        dfa.add_edge(q3, 'a', q3, ());
        dfa.add_edge(q3, 'b', q3, ());
        dfa.add_edge(q3, 'c', q0, ());
        dfa
    }

    #[test]
    fn lstar_for_dfa() {
        let target = test_dfa();
        let oracle = DFAOracle::new(&target);

        let mut lstar = super::LStar::unlogged(oracle, target.alphabet().clone());

        let learned = lstar.infer();
        assert!(learned.equivalent(&target));
    }

    #[test]
    fn lstar_logged() {
        let target = test_dfa();
        let oracle = DFAOracle::new(&target);
        let mut lstar = super::LStar::logged(oracle, target.alphabet().clone());
        let learned = lstar.infer();
        let logs = lstar.logs();
        for ex in logs.examples() {
            println!("{:?}", ex);
        }
    }
}
