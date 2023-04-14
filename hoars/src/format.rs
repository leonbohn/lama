use std::{fmt::Display, ops::Deref};

use crate::Id;

/// Represents a conjunction over states of a HOA automaton, this
/// is mostly used as the initial state of the automaton.
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

/// An atomic proposition is named by a string.
pub type AtomicProposition = String;

/// Aliases are also named by a string.
pub type AliasName = String;

/// An acceptance atom can be used to build an acceptance condition,
/// each atom is either a positive or a negative acceptance set
/// identifier.
#[derive(Debug, PartialEq, Eq, Clone)]
#[allow(missing_docs)]
pub enum AcceptanceAtom {
    Positive(Id),
    Negative(Id),
}

/// An acceptance signature is a vector of acceptance set
/// identifiers, it is associated with an edge.
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

/// An acceptance condition is a positive boolean expression over
/// [`AcceptanceAtom`]s.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AcceptanceCondition {
    /// Represents that the given atom should appear finitely often.
    Fin(AcceptanceAtom),
    /// The given atom should appear infinitely often.
    Inf(AcceptanceAtom),
    /// Represents a conjunction of two acceptance conditions.
    And(Box<AcceptanceCondition>, Box<AcceptanceCondition>),
    /// Represents a disjunction of two acceptance conditions.
    Or(Box<AcceptanceCondition>, Box<AcceptanceCondition>),
    /// A constant boolean value.
    Boolean(bool),
}

/// Represents the name of a type of acceptance condition.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AcceptanceName {
    /// Büchi acceptance, implies that there is only one
    /// acceptance set. Is satisfied if an edge/state of the
    /// acceptance set appears infinitely often.
    Buchi,
    /// Generalized Büchi consists of multiple Büchi acceptance
    /// conditions. It is satisfied if all of the Büchi conditions
    /// are satsified.
    GeneralizedBuchi,
    /// Co-Büchi conditions are dual to Büchi conditions, they are
    /// satisfied if no edge/state from the acceptance set appears
    /// infinitely often.
    CoBuchi,
    /// Generalized co-Büchi conditions are dual to generalized
    /// Büchi conditions, see [`AcceptanceName::GeneralizedBuchi`].
    GeneralizedCoBuchi,
    /// A Streett condition consists of a set of pairs of acceptance
    /// sets. It is dual to a [`AcceptanceName::Rabin`] condition
    /// and satisfied if for each pair (X, Y) in the condition holds
    /// that if X appears infinitely often, then Y also appears
    /// infinitely often.
    Streett,
    /// A Rabin condition is dual to a [`AcceptanceName::Streett`]
    /// condition, it also consists of a set of pairs (X,Y) of
    /// acceptance sets. It is satisfied if there exists a pair
    /// (X, Y) in the condition such that no set from X appears
    /// infinitely often and one set from Y appears infinitely
    /// often.
    Rabin,
    /// A generalized Rabin condition is a set of [`AcceptanceName::Rabin`]
    /// conditions and it is satisfied of all of the Rabin conditions
    /// are satisfied.
    GeneralizedRabin,
    /// A parity (or Mostowski) condition associates with each
    /// state/transition a priority, i.e. a non-negative integer.
    /// It is satisfied if the least priority that appears infinitely
    /// often is even. There is also max even, min odd and max odd
    /// variants of this condition, but they are all equivalent
    /// in terms of expressiveness.
    Parity,
    /// Represents a condition where everything is accepted.
    All,
    /// A condition where nothing is accepted.
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

/// Represents properties of an automaton. For more information
/// see the documentation of the [HOA format](https://adl.github.io/hoaf/#properties).
#[derive(Debug, PartialEq, Eq, Clone)]
#[allow(missing_docs)]
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

/// Used to give additional (human-readable) and optional
/// information about the acceptance condition, more information
/// can be obtained in the [HOA docs](https://adl.github.io/hoaf/#acc-name).
#[derive(Debug, PartialEq, Eq, Clone)]
#[allow(missing_docs)]
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
