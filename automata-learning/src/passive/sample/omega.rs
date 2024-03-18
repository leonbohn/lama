use std::collections::VecDeque;

use automata::{prelude::*, word::LinearWord, Map, Set};
use itertools::Itertools;
use tracing::{debug, trace};

use crate::passive::{
    sprout::{
        iteration_consistency_conflicts, prefix_consistency_conflicts, sprout, SeparatesIdempotents,
    },
    ClassOmegaSample, Sample,
};

use super::{OmegaSample, SplitOmegaSample};

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
    OmegaWordParseError(ReducedParseError),
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

impl TryFrom<&str> for OmegaSample<CharAlphabet, bool> {
    type Error = OmegaSampleParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::try_from(value.lines().map(|s| s.to_string()).collect_vec())
    }
}

impl TryFrom<Vec<String>> for OmegaSample<CharAlphabet, bool> {
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
                    let parsed = ReducedOmegaWord::try_from(word.as_str())
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
            let parsed = ReducedOmegaWord::try_from(word.as_str())
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

impl<A: Alphabet> OmegaSample<A, bool> {
    /// Creates a new `OmegaSample` from an alphabet as well as two iterators, one
    /// over positive words and one over negative words.
    pub fn new_omega_from_pos_neg<
        W: Into<ReducedOmegaWord<A::Symbol>>,
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
    pub fn positive_periodic(&self) -> impl Iterator<Item = PeriodicOmegaWord<A::Symbol>> + '_ {
        self.positive_words()
            .filter_map(|w| PeriodicOmegaWord::try_from(w.clone()).ok())
    }

    /// Returns an iterator over the negative periodic words in the sample.
    pub fn negative_periodic(&self) -> impl Iterator<Item = PeriodicOmegaWord<A::Symbol>> + '_ {
        self.negative_words()
            .filter_map(|w| PeriodicOmegaWord::try_from(w.clone()).ok())
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
        sprout(prefix_consistency_conflicts(self), vec![], true)
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
    positive: Set<PeriodicOmegaWord<A::Symbol>>,
    negative: Set<PeriodicOmegaWord<A::Symbol>>,
}

impl<A: Alphabet> PeriodicOmegaSample<A> {
    /// Gives an iterator over all positive periodic words in the sample.
    pub fn positive(&self) -> impl Iterator<Item = &PeriodicOmegaWord<A::Symbol>> + '_ {
        self.positive.iter()
    }

    /// Gives an iterator over all negative periodic words in the sample.
    pub fn negative(&self) -> impl Iterator<Item = &PeriodicOmegaWord<A::Symbol>> + '_ {
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
    pub fn classify<W: Into<PeriodicOmegaWord<A::Symbol>>>(&self, word: W) -> Option<bool> {
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
    pub fn contains<W: Into<PeriodicOmegaWord<A::Symbol>>>(&self, word: W) -> bool {
        self.classify(word).is_some()
    }
}

impl<A: Alphabet, C: Color> OmegaSample<A, C> {
    /// Create a new sample of infinite words. The words in the sample are given as an iterator yielding (word, color) pairs.
    pub fn new_omega<W: Into<ReducedOmegaWord<A::Symbol>>, J: IntoIterator<Item = (W, C)>>(
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
        let mut split = Map::default();
        split.insert(
            initial,
            ClassOmegaSample::new(cong, Class::epsilon(), self.clone()),
        );
        let mut queue: VecDeque<_> = self
            .words
            .iter()
            .map(|(w, c)| (initial, w.reduced(), c))
            .collect();

        while let Some((state, word, color)) = queue.pop_front() {
            trace!("Processing word {:?} in state {}", word, state);
            let (sym, suffix) = word.pop_first();
            // unwrap okay because words are infinite
            if let Some(reached) = cong.successor_index(state, sym) {
                trace!("\tReached successor {reached}");
                if split
                    .entry(reached)
                    .or_insert_with(|| {
                        ClassOmegaSample::empty(
                            cong,
                            cong.state_color(reached)
                                .expect("We assume every state to have a color")
                                .class()
                                .clone(),
                            self.alphabet.clone(),
                        )
                    })
                    .sample_mut()
                    .words
                    .insert(suffix.reduced(), color.clone())
                    .is_none()
                {
                    trace!("Added word {:?} to state {}", suffix, reached);
                    queue.push_back((reached, suffix.reduced(), color));
                }
            }
        }

        SplitOmegaSample::new(cong, split)
    }
}
