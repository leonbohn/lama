use std::{fmt::Display, ops::Deref};

use crate::Id;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StateConjunction(pub(crate) Vec<crate::Id>);

impl StateConjunction {
    /// Attempts to get the singleton element of the state conjunction, if it exists.
    /// Returns `None` if the state conjunction is not a singleton.
    /// This is useful when dealing with non-alternating automata.
    pub fn get_singleton(&self) -> Option<Id> {
        if self.0.len() == 1 {
            Some(self.0[0])
        } else {
            None
        }
    }
}

pub type AtomicProposition = String;

pub type AliasName = String;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AcceptanceAtom {
    Positive(Id),
    Negative(Id),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AcceptanceSignature(pub(crate) Vec<crate::Id>);

impl AcceptanceSignature {
    /// Tries to get the singleton element of the acceptance signature, if it exists.
    /// Returns `None` if the acceptance signature is not a singleton.
    pub fn get_singleton(&self) -> Option<Option<Id>> {
        if self.len() == 0 {
            Some(None)
        } else if self.len() == 1 {
            Some(Some(self[0]))
        } else {
            None
        }
    }
}

impl Deref for AcceptanceSignature {
    type Target = Vec<crate::Id>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AcceptanceCondition {
    Fin(AcceptanceAtom),
    Inf(AcceptanceAtom),
    And(Box<AcceptanceCondition>, Box<AcceptanceCondition>),
    Or(Box<AcceptanceCondition>, Box<AcceptanceCondition>),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AcceptanceName {
    Buchi,
    GeneralizedBuchi,
    CoBuchi,
    GeneralizedCoBuchi,
    Streett,
    Rabin,
    GeneralizedRabin,
    Parity,
    All,
    None,
}

impl TryFrom<String> for AcceptanceName {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "Buchi" => Ok(AcceptanceName::Buchi),
            "generalized-Buchi" => Ok(AcceptanceName::GeneralizedBuchi),
            "co-Buchi" => Ok(AcceptanceName::CoBuchi),
            "generalized-co-Buchi" => Ok(AcceptanceName::GeneralizedCoBuchi),
            "Streett" => Ok(AcceptanceName::Streett),
            "Rabin" => Ok(AcceptanceName::Rabin),
            "generalized-Rabin" => Ok(AcceptanceName::GeneralizedRabin),
            "parity" => Ok(AcceptanceName::Parity),
            "all" => Ok(AcceptanceName::All),
            "none" => Ok(AcceptanceName::None),
            val => Err(format!("Unknown acceptance type: {}", val)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Property {
    StateLabels,
    TransLabels,
    ImplicitLabels,
    ExplicitLabels,
    StateAcceptance,
    TransitionAcceptance,
    UniversalBranching,
    NoUniversalBranching,
    Deterministic,
    Complete,
    Unambiguous,
    StutterInvariant,
    Weak,
    VeryWeak,
    InherentlyWeak,
    Terminal,
    Tight,
    Colored,
}

impl TryFrom<String> for Property {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "state-labels" => Ok(Property::StateLabels),
            "trans-labels" => Ok(Property::TransLabels),
            "implicit-labels" => Ok(Property::ImplicitLabels),
            "explicit-labels" => Ok(Property::ExplicitLabels),
            "state-acc" => Ok(Property::StateAcceptance),
            "trans-acc" => Ok(Property::TransitionAcceptance),
            "univ-branch" => Ok(Property::UniversalBranching),
            "no-univ-branch" => Ok(Property::NoUniversalBranching),
            "deterministic" => Ok(Property::Deterministic),
            "complete" => Ok(Property::Complete),
            "unambiguous" => Ok(Property::Unambiguous),
            "stutter-invariant" => Ok(Property::StutterInvariant),
            "weak" => Ok(Property::Weak),
            "very-weak" => Ok(Property::VeryWeak),
            "inherently-weak" => Ok(Property::InherentlyWeak),
            "terminatl" => Ok(Property::Terminal),
            "tight" => Ok(Property::Tight),
            "colored" => Ok(Property::Colored),
            unknown => Err(format!("{} is not a valid property", unknown)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AcceptanceInfo {
    Int(crate::Id),
    Identifier(String),
}

impl Display for AcceptanceAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AcceptanceAtom::Positive(id) => write!(f, "{}", id),
            AcceptanceAtom::Negative(id) => write!(f, "!{}", id),
        }
    }
}

impl Display for AcceptanceCondition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AcceptanceCondition::Fin(id) => write!(f, "Fin({})", id),
            AcceptanceCondition::Inf(id) => write!(f, "Inf({})", id),
            AcceptanceCondition::And(left, right) => write!(f, "({} & {})", left, right),
            AcceptanceCondition::Or(left, right) => write!(f, "({} | {})", left, right),
            AcceptanceCondition::Boolean(val) => write!(f, "{}", if *val { "T" } else { "F" }),
        }
    }
}
