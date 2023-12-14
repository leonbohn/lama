use std::fmt::Display;

use itertools::Itertools;

use crate::{
    AcceptanceAtom, AcceptanceCondition, AcceptanceInfo, AcceptanceName, AcceptanceSignature,
    AliasName, Edge, HeaderItem, HoaAutomaton, HoaBool, Label, Property, State, StateConjunction,
};

pub fn to_hoa(aut: &HoaAutomaton) -> String {
    aut.header()
        .into_iter()
        .map(|header_item| header_item.to_string())
        .chain(std::iter::once("--BODY--".to_string()))
        .chain(aut.body().into_iter().map(|state| state.to_string()))
        .chain(std::iter::once("--END--".to_string()))
        .join("\n")
}

impl Display for HeaderItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeaderItem::Version(version) => write!(f, "HOA: {}", version),
            HeaderItem::States(number) => write!(f, "States: {}", number),
            HeaderItem::Start(state_conj) => write!(f, "Start: {}", state_conj),
            HeaderItem::AP(aps) => write!(
                f,
                "AP: {} {}",
                aps.len(),
                aps.iter().map(|ap| format!("\"{}\"", ap)).join(" ")
            ),
            HeaderItem::Alias(alias_name, alias_expression) => {
                write!(f, "Alias: {} {}", alias_name, alias_expression)
            }
            HeaderItem::Acceptance(number_sets, condition) => {
                write!(f, "Acceptance: {} {}", number_sets, condition)
            }
            HeaderItem::AcceptanceName(identifier, vec_info) => {
                write!(f, "acc-name: {} {}", identifier, vec_info.iter().join(" "))
            }
            HeaderItem::Tool(name, options) => {
                write!(f, "tool: {} {}", name, options.iter().join(" "))
            }
            HeaderItem::Name(name) => write!(f, "name: {}", name),
            HeaderItem::Properties(properties) => {
                write!(f, "properties: {}", properties.iter().join(" "))
            }
        }
    }
}

impl Display for AcceptanceInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AcceptanceInfo::Int(integer) => write!(f, "{}", integer),
            AcceptanceInfo::Identifier(identifier) => write!(f, "{}", identifier),
        }
    }
}

impl Display for Property {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Property::StateLabels => "state-labels",
                Property::TransLabels => "trans-labels",
                Property::ImplicitLabels => "implicit-labels",
                Property::ExplicitLabels => "explicit-labels",
                Property::StateAcceptance => "state-acc",
                Property::TransitionAcceptance => "trans-acc",
                Property::UniversalBranching => "univ-branch",
                Property::NoUniversalBranching => "no-univ-branch",
                Property::Deterministic => "deterministic",
                Property::Complete => "complete",
                Property::Unambiguous => "unabmiguous",
                Property::StutterInvariant => "stutter-invariant",
                Property::Weak => "weak",
                Property::VeryWeak => "very-weak",
                Property::InherentlyWeak => "inherently-weak",
                Property::Terminal => "terminal",
                Property::Tight => "tight",
                Property::Colored => "colored",
            }
        )
    }
}

impl Display for AcceptanceName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AcceptanceName::Buchi => "Buchi",
                AcceptanceName::GeneralizedBuchi => "generalized-Buchi",
                AcceptanceName::CoBuchi => "co-Buchi",
                AcceptanceName::GeneralizedCoBuchi => "generalized-co-Buchi",
                AcceptanceName::Streett => "Streett",
                AcceptanceName::Rabin => "Rabin",
                AcceptanceName::GeneralizedRabin => "generalized-Rabin",
                AcceptanceName::Parity => "parity",
                AcceptanceName::All => "all",
                AcceptanceName::None => "none",
            }
        )
    }
}

impl Display for AcceptanceAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AcceptanceAtom::Positive(id) => write!(f, "{}", id),
            AcceptanceAtom::Negative(id) => write!(f, "!{}", id),
        }
    }
}

impl Display for HoaBool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if self.0 { "t" } else { "f" })
    }
}

impl Display for AcceptanceCondition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AcceptanceCondition::Fin(id) => write!(f, "Fin({})", id),
            AcceptanceCondition::Inf(id) => write!(f, "Inf({})", id),
            AcceptanceCondition::And(left, right) => write!(f, "({} & {})", left, right),
            AcceptanceCondition::Or(left, right) => write!(f, "({} | {})", left, right),
            AcceptanceCondition::Boolean(val) => write!(f, "{}", val),
        }
    }
}

impl Display for AliasName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

impl Display for StateConjunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().map(|s| s.to_string()).join(" & "))
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0)
    }
}

impl Display for AcceptanceSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_empty() {
            return Ok(());
        }
        write!(f, "{{{}}}", self.0.iter().join(" "))
    }
}

impl Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.0, self.1, self.2)
    }
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(acc) = &self.1 {
            writeln!(f, "State: {} \"{}\"", self.0, acc)?;
        } else {
            writeln!(f, "State: {}", self.0)?;
        }
        for edge in &self.2 {
            writeln!(f, "{}", edge)?;
        }
        Ok(())
    }
}
