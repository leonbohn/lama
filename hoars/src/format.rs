use crate::Id;

pub type StateConjunction = Vec<crate::Id>;

pub type AtomicProposition = String;

pub type AliasName = String;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AcceptanceAtom {
    Positive(Id),
    Negative(Id),
}

pub type AcceptanceSignature = Vec<crate::Id>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LabelExpression {
    Boolean(bool),
    Integer(u32),
    Alias(AliasName),
    Not(Box<LabelExpression>),
    And(Box<LabelExpression>, Box<LabelExpression>),
    Or(Box<LabelExpression>, Box<LabelExpression>),
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
