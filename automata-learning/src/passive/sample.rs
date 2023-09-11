use std::{
    cell::RefCell,
    collections::{BTreeSet, VecDeque},
    fmt::Debug,
    hash::Hash,
};

use automata::{
    alphabet::{self, Simple, Symbol},
    congurence::{IndexesRightCongruence, FORC},
    ts::{operations::Product, FiniteState, HasStates, Pointed, Sproutable},
    word::{Normalized, NormalizedParseError, NormalizedPeriodic, OmegaWord, RawSymbols},
    Acceptor, Alphabet, Class, Color, FiniteLength, HasLength, InfiniteLength, Length, Map,
    MooreMachine, RightCongruence, Set, TransitionSystem, Word, DFA,
};
use itertools::Itertools;
use tracing::{debug, trace};

use crate::passive::sprout::iteration_consistency_conflicts;

use super::sprout::{omega_sprout_conflicts, prefix_consistency_conflicts, SeparatesIdempotents};

/// Represents a finite sample, which is a pair of positive and negative instances.
#[derive(Clone, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct Sample<A: Alphabet, L: Length, C: Color = bool> {
    pub alphabet: A,
    pub words: Map<Normalized<A::Symbol, L>, C>,
}

/// An `OmegaSample` is just a sample that contains infinite words.
pub type OmegaSample<A, C = bool> = Sample<A, InfiniteLength, C>;

/// Abstracts the types of errors that can occur when parsing an `OmegaSample` from a string.
#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum OmegaSampleParseError {
    MissingHeader,
    MissingAlphabet,
    MissingDelimiter,
    MalformedAlphabetSymbol,
    Inconsistent(String),
    MalformedSample,
    OmegaWordParseError(NormalizedParseError),
}

impl std::fmt::Display for OmegaSampleParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OmegaSampleParseError::MissingHeader => write!(f, "Missing header"),
            OmegaSampleParseError::MissingAlphabet => write!(f, "Missing alphabet"),
            OmegaSampleParseError::MissingDelimiter => write!(f, "Missing delimiter"),
            OmegaSampleParseError::MalformedAlphabetSymbol => write!(f, "Malformed alphabet"),
            OmegaSampleParseError::MalformedSample => write!(f, "Malformed sample"),
            OmegaSampleParseError::OmegaWordParseError(err) => {
                write!(f, "Could not parse omega-word: {}", err)
            }
            OmegaSampleParseError::Inconsistent(word) => write!(
                f,
                "Inconsistent sample, {word} is both positive and negative"
            ),
        }
    }
}

impl TryFrom<&str> for OmegaSample<Simple, bool> {
    type Error = OmegaSampleParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::try_from(value.lines().map(|s| s.to_string()).collect_vec())
    }
}

impl TryFrom<Vec<String>> for OmegaSample<Simple, bool> {
    type Error = OmegaSampleParseError;

    fn try_from(value: Vec<String>) -> Result<Self, Self::Error> {
        let mut lines = value
            .into_iter()
            .filter(|line| !line.trim().is_empty() && !line.starts_with('#'));
        if lines.next().unwrap_or_default().trim() != "omega" {
            return Err(OmegaSampleParseError::MissingHeader);
        }

        let alphabet = lines
            .next()
            .and_then(|line| {
                line.split_once(':')
                    .map(|(header, symbols)| (header.to_string(), symbols.to_string()))
            })
            .map(|(header, symbols)| {
                if header.trim() == "alphabet" {
                    symbols.split(',').try_fold(Vec::new(), |mut acc, x| {
                        let sym = x.trim();
                        if sym.len() != 1 {
                            Err(OmegaSampleParseError::MalformedAlphabetSymbol)
                        } else {
                            acc.push(sym.chars().next().unwrap());
                            Ok(acc)
                        }
                    })
                } else {
                    Err(OmegaSampleParseError::MissingAlphabet)
                }
            })
            .ok_or(OmegaSampleParseError::MissingDelimiter)??
            .into();

        if lines.next().unwrap_or_default().trim() != "positive:" {
            return Err(OmegaSampleParseError::MalformedSample);
        }

        let mut words = Map::default();
        'positive: loop {
            match lines.next() {
                Some(word) => {
                    trace!("Parsing positive word \"{word}\"");
                    let trim = word.trim();
                    if trim.is_empty() || trim.starts_with('#') || trim == "negative:" {
                        break 'positive;
                    }
                    let parsed = Normalized::try_from(word.as_str())
                        .map_err(OmegaSampleParseError::OmegaWordParseError)?;
                    if let Some(old_classification) = words.insert(parsed, true) {
                        debug!("Duplicate positive word found");
                    }
                }
                None => return Err(OmegaSampleParseError::MalformedSample),
            }
        }
        for word in lines {
            trace!("Parsing negative word \"{word}\"");
            let parsed = Normalized::try_from(word.as_str())
                .map_err(OmegaSampleParseError::OmegaWordParseError)?;
            if let Some(old_classification) = words.insert(parsed, false) {
                if old_classification {
                    return Err(OmegaSampleParseError::Inconsistent(word));
                }
                debug!("Duplicate negative word found");
            };
        }

        Ok(Sample::new_omega(alphabet, words))
    }
}

impl<A: Alphabet, L: Length> Sample<A, L, bool> {
    /// Gives an iterator over all positive words in the sample.
    pub fn positive_words(&self) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words_with_color(true)
    }

    /// Gives an iterator over all negative words in the sample.
    pub fn negative_words(&self) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words_with_color(false)
    }
}

impl<A: Alphabet, L: Length, C: Color> Sample<A, L, C> {
    /// Gives an iterator over all words in the sample.
    pub fn words(&self) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words.keys()
    }

    /// Classifying a word returns the color that is associated with it.
    pub fn classify<W: Into<Normalized<A::Symbol, L>>>(&self, word: W) -> Option<C> {
        let word = word.into();
        self.words.get(&word).cloned()
    }

    /// Checks whether a word is contained in the sample.
    pub fn contains(&self, word: &Normalized<A::Symbol, L>) -> bool {
        self.words.contains_key(word)
    }

    /// Gives an iterator over all words in the sample with the associated color.
    pub fn words_with_color(
        &self,
        color: C,
    ) -> impl Iterator<Item = &'_ Normalized<A::Symbol, L>> + '_ {
        self.words
            .iter()
            .filter_map(move |(w, c)| if *c == color { Some(w) } else { None })
    }
}

impl<A: Alphabet, C: Color> Sample<A, FiniteLength, C> {
    /// Create a new sample of finite words from the given alphabet and iterator over annotated words. The sample is given
    /// as an iterator over its symbols. The words are given as an iterator of pairs (word, color).
    pub fn new_finite<I: IntoIterator<Item = A::Symbol>, J: IntoIterator<Item = (I, C)>>(
        alphabet: A,
        words: J,
    ) -> Self {
        let words = words
            .into_iter()
            .map(|(word, color)| (Normalized::new_finite(word), color))
            .collect();
        Self { alphabet, words }
    }
}
impl<A: Alphabet> OmegaSample<A, bool> {
    /// Creates a new `OmegaSample` from an alphabet as well as two iterators, one
    /// over positive words and one over negative words.
    pub fn new_omega_from_pos_neg<
        W: Into<Normalized<A::Symbol, InfiniteLength>>,
        I: IntoIterator<Item = W>,
        J: IntoIterator<Item = W>,
    >(
        alphabet: A,
        positive: I,
        negative: J,
    ) -> Self {
        Self {
            alphabet,
            words: positive
                .into_iter()
                .map(|w| (w.into(), true))
                .chain(negative.into_iter().map(|w| (w.into(), false)))
                .collect(),
        }
    }

    /// Returns an iterator over the positive periodic words in the sample.
    pub fn positive_periodic(&self) -> impl Iterator<Item = NormalizedPeriodic<A::Symbol>> + '_ {
        self.positive_words()
            .filter_map(|w| NormalizedPeriodic::try_from(w.clone()).ok())
    }

    /// Returns an iterator over the negative periodic words in the sample.
    pub fn negative_periodic(&self) -> impl Iterator<Item = NormalizedPeriodic<A::Symbol>> + '_ {
        self.negative_words()
            .filter_map(|w| NormalizedPeriodic::try_from(w.clone()).ok())
    }

    /// Computes a `PeriodicOmegaSample` containing only the periodic words in the sample.
    pub fn to_periodic_sample(&self) -> PeriodicOmegaSample<A> {
        PeriodicOmegaSample {
            alphabet: self.alphabet.clone(),
            positive: self.positive_periodic().collect(),
            negative: self.negative_periodic().collect(),
        }
    }

    /// Computes the [`RightCongruence`] underlying the sample.
    pub fn infer_right_congruence(&self) -> RightCongruence<A> {
        omega_sprout_conflicts(prefix_consistency_conflicts(self), (), true)
    }

    /// Computes the [`FORC`] underlying the sample.
    pub fn infer_forc(&self) -> FORC<A> {
        let cong = self.infer_right_congruence();
        let split_sample = self.split(&cong);

        let conflict_relations: Map<_, _> = split_sample
            .classes()
            .map(|c| {
                (
                    c.clone(),
                    iteration_consistency_conflicts(&split_sample, c.clone()),
                )
            })
            .collect();

        let progress = conflict_relations
            .into_iter()
            .map(|(c, conflicts)| {
                (
                    c.clone(),
                    omega_sprout_conflicts(
                        conflicts,
                        SeparatesIdempotents::new(split_sample.get(&c).expect("This must exist")),
                        false,
                    ),
                )
            })
            .collect_vec();
        FORC::from_iter(cong, progress)
    }

    /// Returns the positive size, i.e. the number of positive words.
    pub fn positive_size(&self) -> usize {
        self.words_with_color(true).count()
    }

    /// Returns the negative size, i.e. the number of negative words.
    pub fn negative_size(&self) -> usize {
        self.words_with_color(false).count()
    }
}

/// A [`PeriodicOmegaSample`] is an omega sample containing only periodic words.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PeriodicOmegaSample<A: Alphabet> {
    alphabet: A,
    positive: BTreeSet<NormalizedPeriodic<A::Symbol>>,
    negative: BTreeSet<NormalizedPeriodic<A::Symbol>>,
}

impl<A: Alphabet> PeriodicOmegaSample<A> {
    /// Gives an iterator over all positive periodic words in the sample.
    pub fn positive(&self) -> impl Iterator<Item = &NormalizedPeriodic<A::Symbol>> + '_ {
        self.positive.iter()
    }

    /// Gives an iterator over all negative periodic words in the sample.
    pub fn negative(&self) -> impl Iterator<Item = &NormalizedPeriodic<A::Symbol>> + '_ {
        self.negative.iter()
    }

    /// Gives the size i.e. number of positive words.
    pub fn positive_size(&self) -> usize {
        self.positive.len()
    }

    /// Gives the size i.e. number of negative words.
    pub fn negative_size(&self) -> usize {
        self.negative.len()
    }

    /// Returns the size i.e. number of words.
    pub fn size(&self) -> usize {
        self.positive_size() + self.negative_size()
    }

    /// Returns a reference to the alphabet underlying the sample.
    pub fn alphabet(&self) -> &A {
        &self.alphabet
    }

    /// Classify the given word, i.e. return `true` if it is a positive word, `false` if it is a negative word and `None` if it is neither.
    pub fn classify<W: Into<NormalizedPeriodic<A::Symbol>>>(&self, word: W) -> Option<bool> {
        let word = word.into();
        if self.positive.contains(&word) {
            Some(true)
        } else if self.negative.contains(&word) {
            Some(false)
        } else {
            None
        }
    }

    /// Check whether the given word is contained in the sample.
    pub fn contains<W: Into<NormalizedPeriodic<A::Symbol>>>(&self, word: W) -> bool {
        self.classify(word).is_some()
    }
}

/// An [`OmegaSample`] restricted/split onto one [`Class`] of a [`RightCongruence`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassOmegaSample<'a, A: Alphabet, C: Color> {
    congruence: &'a RightCongruence<A>,
    class: Class<A::Symbol>,
    sample: Sample<A, InfiniteLength, C>,
}

impl<'a, A: Alphabet, C: Color> std::ops::Deref for ClassOmegaSample<'a, A, C> {
    type Target = Sample<A, InfiniteLength, C>;

    fn deref(&self) -> &Self::Target {
        &self.sample
    }
}

impl<'a, A: Alphabet, C: Color> std::ops::DerefMut for ClassOmegaSample<'a, A, C> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.sample
    }
}

impl<'a, A: Alphabet, C: Color> ClassOmegaSample<'a, A, C> {
    /// Creates a new [`ClassOmegaSample`] from a [`RightCongruence`], a [`Class`] and a [`Sample`].
    pub fn new(
        congruence: &'a RightCongruence<A>,
        class: Class<A::Symbol>,
        sample: Sample<A, InfiniteLength, C>,
    ) -> Self {
        Self {
            congruence,
            class,
            sample,
        }
    }

    /// Creates an empty [`ClassOmegaSample`] from a [`RightCongruence`], a [`Class`] and an alphabet.
    pub fn empty(congruence: &'a RightCongruence<A>, class: Class<A::Symbol>, alphabet: A) -> Self {
        Self {
            congruence,
            class,
            sample: Sample {
                alphabet,
                words: Map::default(),
            },
        }
    }
}

/// Represents a right congruence relation together with a collection of split samples, one
/// associated with each class of the congruence.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SplitOmegaSample<'a, A: Alphabet, C: Color> {
    congruence: &'a RightCongruence<A>,
    split: Map<usize, ClassOmegaSample<'a, A, C>>,
}

impl<'a, A: Alphabet, C: Color> SplitOmegaSample<'a, A, C> {
    /// Creates a new object from the given congruence and the split
    pub fn new(
        congruence: &'a RightCongruence<A>,
        split: Map<usize, ClassOmegaSample<'a, A, C>>,
    ) -> Self {
        Self { congruence, split }
    }

    /// Obtain a reference to the split sample for the given class/index.
    pub fn get<I: IndexesRightCongruence<A>>(
        &self,
        index: I,
    ) -> Option<&ClassOmegaSample<'a, A, C>> {
        index
            .to_index(self.congruence)
            .and_then(|idx| self.split.get(&idx))
    }

    /// Obtains an iterator over all classes in the split sample.
    pub fn classes(&self) -> impl Iterator<Item = &'_ Class<A::Symbol>> + '_ {
        self.split.values().map(|sample| &sample.class)
    }

    /// Returns a reference to the underlying congruence.
    pub fn cong(&self) -> &'a RightCongruence<A> {
        self.congruence
    }
}

impl<A: Alphabet, C: Color> OmegaSample<A, C> {
    /// Create a new sample of infinite words. The alphabet is given as something which implements [`RawSymbols`]. The words
    /// in the sample are given as an iterator yielding (word, color) pairs.
    pub fn new_omega<
        W: Into<Normalized<A::Symbol, InfiniteLength>>,
        J: IntoIterator<Item = (W, C)>,
    >(
        alphabet: A,
        words: J,
    ) -> Self {
        let words = words
            .into_iter()
            .map(|(word, color)| (word.into(), color))
            .collect();

        Self { alphabet, words }
    }

    /// Splits the sample into a map of [`ClassOmegaSample`]s, one for each class of the underlying [`RightCongruence`].
    pub fn split<'a>(&self, cong: &'a RightCongruence<A>) -> SplitOmegaSample<'a, A, C> {
        debug_assert!(
            cong.size() > 0,
            "Makes only sense for non-empty congruences"
        );
        let initial = cong.initial();
        // take self as is for epsilon
        let mut spilt = Map::default();
        spilt.insert(
            initial,
            ClassOmegaSample::new(cong, Class::epsilon(), self.clone()),
        );
        let mut queue: VecDeque<_> = self
            .words
            .iter()
            .map(|(w, c)| (initial, w.normalized(), c))
            .collect();

        while let Some((state, word, color)) = queue.pop_front() {
            trace!("Processing word {:?} in state {}", word, state);
            let (sym, suffix) = word.pop_first();
            // unwrap okay because words are infinite
            if let Some(reached) = cong.successor_index(state, sym) {
                trace!("\tReached successor {reached}");
                if spilt
                    .entry(reached)
                    .or_insert_with(|| {
                        ClassOmegaSample::empty(
                            cong,
                            cong.state_color(reached)
                                .expect("We assume every state to have a color"),
                            self.alphabet.clone(),
                        )
                    })
                    .sample
                    .words
                    .insert(suffix.normalized(), color.clone())
                    .is_none()
                {
                    trace!("Added word {:?} to state {}", suffix, reached);
                    queue.push_back((reached, suffix.normalized(), color));
                }
            }
        }

        SplitOmegaSample::new(cong, spilt)
    }
}

impl<A, L, C> Debug for Sample<A, L, C>
where
    A: Alphabet + Debug,
    L: Length + Debug,
    C: Color + Debug,
    Normalized<A::Symbol, L>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Sample with alphabet {:?}", self.alphabet)?;
        for (word, color) in &self.words {
            write!(f, "\n\t{:?}\t{:?}", color, word)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use automata::{
        alphabet::Simple,
        npw, nupw, simple,
        ts::{finite::ReachedColor, Congruence, HasStates, Sproutable},
        upw, Acceptor, HasLength, Pointed, RightCongruence, TransitionSystem,
    };
    use itertools::Itertools;
    use tracing::info;
    use tracing_test::traced_test;

    use crate::Sample;

    use super::Normalized;

    #[test]
    fn parse_sample() {
        let sample_str = r#"omega
        alphabet: a, b
        positive:
        a
        b,a
        aab
        baa
        negative:
        b
        ab
        abb"#;

        let sample = match Sample::try_from(sample_str) {
            Ok(s) => s,
            Err(e) => panic!("Error parsing sample: {:?}", e),
        };

        assert_eq!(sample.alphabet, simple!('a', 'b'));
        assert_eq!(sample.positive_size(), 4);
        assert_eq!(sample.negative_size(), 3);
        assert_eq!(sample.classify(nupw!("ab")), Some(false));
    }

    #[test]
    fn to_periodic_sample() {
        let alphabet = simple!('a', 'b');
        // represents congruence e ~ b ~ aa ~\~ a ~ ab
        let sample = Sample::new_omega_from_pos_neg(
            alphabet,
            [nupw!("ab", "b"), nupw!("a", "b"), nupw!("bbbbbb")],
            [nupw!("aa")],
        );
        let periodic_sample = sample.to_periodic_sample();
        assert_eq!(periodic_sample.positive_size(), 1);
        assert_eq!(periodic_sample.negative_size(), 1);
        assert!(periodic_sample.contains(npw!("b")));
        assert!(periodic_sample.contains(npw!("a")));
        assert_eq!(periodic_sample.classify(npw!("bb")), Some(true));
    }

    #[test]
    #[traced_test]
    fn split_up_sample() {
        let alphabet = simple!('a', 'b');
        // represents congruence e ~ b ~ aa ~\~ a ~ ab
        let sample = Sample::new_omega(
            alphabet.clone(),
            vec![
                (("b", 0), true),
                (("abab", 3), true),
                (("abbab", 4), true),
                (("ab", 1), false),
                (("a", 0), false),
            ],
        );
        let cong = sample.infer_right_congruence();
        let split = sample.split(&cong);

        for w in ["b"] {
            assert!(split.get(0).unwrap().contains(&nupw!(w)))
        }

        println!("{:?}", split.get(0).unwrap());
        println!("{:?}", split.get(1).unwrap());
    }

    #[test]
    fn omega_prefix_tree() {
        let mut w = nupw!("aba", "b");
        let x = w.pop_front();

        let words = vec![
            nupw!("aba", "b"),
            nupw!("a"),
            nupw!("ab"),
            nupw!("bba"),
            nupw!("b", "a"),
            nupw!("b"),
            nupw!("aa", "b"),
        ];

        let time_start = std::time::Instant::now();
        let cong = crate::prefixtree::prefix_tree(Simple::from_iter("ab".chars()), words);
        info!(
            "Construction of congruence took {}μs",
            time_start.elapsed().as_micros()
        );

        for (access, mr) in [("aaaa", "aaa"), ("baaa", "ba"), ("bbbbbbbbbb", "bbb")] {
            let expected_state_name = mr.chars().collect_vec().into();
            assert_eq!(
                cong.reached_color(&access),
                Some(ReachedColor(expected_state_name))
            );
        }

        let dfa = cong.map_colors(|_| true);
        for prf in ["aba", "ababbbbbb", "", "aa", "b", "bbabbab"] {
            assert!(dfa.accepts(&prf));
        }
    }
}
