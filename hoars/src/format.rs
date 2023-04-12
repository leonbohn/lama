use std::{fmt::Display, ops::Deref};

use ariadne::Label;

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
    pub fn get_singleton(&self) -> Option<Id> {
        if self.len() == 1 {
            Some(self[0])
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LabelExpression {
    Boolean(bool),
    Integer(u32),
    Alias(AliasName),
    Not(Box<LabelExpression>),
    And(Box<LabelExpression>, Box<LabelExpression>),
    Or(Box<LabelExpression>, Box<LabelExpression>),
}

impl LabelExpression {
    pub fn nnf(&self) -> LabelExpression {
        match self {
            LabelExpression::Not(subexpr) => match subexpr.deref() {
                LabelExpression::Not(subsubexpr) => subsubexpr.nnf(),
                LabelExpression::And(l, r) => LabelExpression::Or(
                    Box::new(LabelExpression::Not(l.clone()).nnf()),
                    Box::new(LabelExpression::Not(r.clone()).nnf()),
                ),
                LabelExpression::Or(l, r) => LabelExpression::And(
                    Box::new(LabelExpression::Not(l.clone()).nnf()),
                    Box::new(LabelExpression::Not(r.clone()).nnf()),
                ),
                otherwise => LabelExpression::Not(Box::new(otherwise.nnf())),
            },
            LabelExpression::And(l, r) => {
                LabelExpression::And(Box::new(l.nnf()), Box::new(r.nnf()))
            }
            LabelExpression::Or(l, r) => LabelExpression::Or(Box::new(l.nnf()), Box::new(r.nnf())),
            otherwise => otherwise.clone(),
        }
    }

    pub fn dnf(&self) -> LabelExpression {
        match self.nnf() {
            LabelExpression::And(l, r) => match (l.dnf(), r.dnf()) {
                (LabelExpression::Or(l1, r1), _) => LabelExpression::Or(
                    Box::new(LabelExpression::And(l1.clone(), r.clone()).dnf()),
                    Box::new(LabelExpression::And(r1.clone(), r.clone()).dnf()),
                ),
                (_, LabelExpression::Or(l2, r2)) => LabelExpression::Or(
                    Box::new(LabelExpression::And(l.clone(), l2.clone()).dnf()),
                    Box::new(LabelExpression::And(r.clone(), r2.clone()).dnf()),
                ),
                (other, wise) => LabelExpression::And(Box::new(other), Box::new(wise)),
            },
            otherwise => otherwise.clone(),
        }
    }
}

impl PartialOrd for LabelExpression {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LabelExpression {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        todo!()
    }
}

impl Display for LabelExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LabelExpression::Boolean(val) => write!(f, "{}", if *val { "T" } else { "F" }),
            LabelExpression::Integer(val) => write!(f, "{}", val),
            LabelExpression::Alias(val) => write!(f, "{}", val),
            LabelExpression::Not(val) => write!(f, "!{}", val),
            LabelExpression::And(lhs, rhs) => write!(f, "({} & {})", lhs, rhs),
            LabelExpression::Or(lhs, rhs) => write!(f, "({} | {})", lhs, rhs),
        }
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

#[cfg(test)]
mod tests {
    #[test]
    fn formula_transformations() {
        use super::*;
        let formula = LabelExpression::Or(
            Box::new(LabelExpression::Not(Box::new(LabelExpression::Or(
                Box::new(LabelExpression::Integer(0)),
                Box::new(LabelExpression::Integer(2)),
            )))),
            Box::new(LabelExpression::Not(Box::new(LabelExpression::Integer(4)))),
        );
        let dnf = formula.dnf();
        println!("{:?}", dnf)
    }
}
