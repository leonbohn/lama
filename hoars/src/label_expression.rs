use std::{fmt::Display, ops::Deref};

use crate::{AliasName, HoaBool, Label};

/// A label expression is a boolean expression over atomic propositions, aliases and boolean values.
/// It is used to label transitions in an automaton.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LabelExpression {
    /// A constant boolean value.
    Boolean(HoaBool),
    /// An integer value representing the atomic proposition.
    Integer(u32),
    /// An alias for a label expression.
    Alias(AliasName),
    /// Negation of a label expression.
    Not(Box<LabelExpression>),
    /// Conjunction of two label expressions.
    And(Box<LabelExpression>, Box<LabelExpression>),
    /// Disjunction of two label expressions.
    Or(Box<LabelExpression>, Box<LabelExpression>),
}

impl LabelExpression {
    /// Turns a label expression into negation normal form.
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

    /// Turns a label expression into disjunctive normal form.
    pub fn dnf(&self) -> LabelExpression {
        match self.nnf() {
            LabelExpression::And(l, r) => match (l.dnf(), r.dnf()) {
                (LabelExpression::Or(l1, r1), _) => LabelExpression::Or(
                    Box::new(LabelExpression::And(l1, r.clone()).dnf()),
                    Box::new(LabelExpression::And(r1, r).dnf()),
                ),
                (_, LabelExpression::Or(l2, r2)) => LabelExpression::Or(
                    Box::new(LabelExpression::And(l, l2).dnf()),
                    Box::new(LabelExpression::And(r, r2).dnf()),
                ),
                (other, wise) => LabelExpression::And(Box::new(other), Box::new(wise)),
            },
            otherwise => otherwise,
        }
    }
}

/// Used to build a reduced form of [`LabelExpression`] that represents a disjunction of conjunctions,
/// see [`DnfLabelExpression`].
#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum LabelConjunct {
    /// A conjunction of atomic propositions.
    Ap(u32),
    /// A conjunction of boolean values.
    Bool(HoaBool),
    /// A negation of an atom
    Not(Box<LabelConjunct>),
}

impl LabelConjunct {
    pub fn to_expression(&self) -> LabelExpression {
        match self {
            LabelConjunct::Ap(ap) => LabelExpression::Integer(*ap),
            LabelConjunct::Bool(b) => LabelExpression::Boolean(b.clone()),
            LabelConjunct::Not(n) => LabelExpression::Not(Box::new(n.to_expression())),
        }
    }
}

impl TryFrom<LabelExpression> for LabelConjunct {
    type Error = String;

    fn try_from(value: LabelExpression) -> Result<Self, Self::Error> {
        match value {
            LabelExpression::Integer(ap) => Ok(LabelConjunct::Ap(ap)),
            LabelExpression::Boolean(b) => Ok(LabelConjunct::Bool(b)),
            LabelExpression::Not(subexpr) => Ok(LabelConjunct::Not(Box::new(
                LabelConjunct::try_from(*subexpr)?,
            ))),
            otherwise => Err(format!(
                "Cannot convert {:?} into a conjunction of atoms",
                otherwise
            )),
        }
    }
}

/// A disjuncion of label expressions. Each of the LabelExpressions in the vector
/// may only contain a conjunction.
#[derive(Debug, PartialEq, Eq, Clone, Hash, Default)]
pub struct DnfLabelExpression(Vec<LabelConjunct>);

impl PartialOrd for DnfLabelExpression {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for DnfLabelExpression {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self.get_aps().min(), other.get_aps().min()) {
            (Some(a), Some(b)) => a.cmp(b),
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => self.get_bools().min().cmp(&other.get_bools().min()),
        }
    }
}

impl IntoIterator for DnfLabelExpression {
    type Item = LabelConjunct;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl DnfLabelExpression {
    pub fn to_label(&self) -> Label {
        Label(
            self.0
                .iter()
                .map(|x| x.to_expression())
                .reduce(|l, r| LabelExpression::Or(Box::new(l), Box::new(r)))
                .unwrap(),
        )
    }

    /// Returns an iterator over the atomic propositions in the disjunction.
    pub fn get_aps(&self) -> impl Iterator<Item = &u32> {
        self.0.iter().filter_map(|x| {
            if let LabelConjunct::Ap(i) = x {
                Some(i)
            } else {
                None
            }
        })
    }

    /// Returns an iterator over the boolean values in the disjunction.
    pub fn get_bools(&self) -> impl Iterator<Item = &bool> {
        self.0.iter().filter_map(|x| {
            if let LabelConjunct::Bool(b) = x {
                Some(&b.0)
            } else {
                None
            }
        })
    }

    /// Returns the number of conjunctions in the disjunction.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if the disjunction contains no conjunctions.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns an iterator over the conjunctions in the disjunction.
    pub fn iter(&self) -> impl Iterator<Item = &LabelConjunct> {
        self.0.iter()
    }

    /// Extends self with the given disjunction.
    pub fn extend<I: IntoIterator<Item = LabelConjunct>>(&mut self, other: I) {
        other.into_iter().for_each(|c| {
            self.0.push(c);
        });
        self.0.sort();
    }

    pub fn append(&mut self, other: LabelConjunct) {
        self.0.push(other);
        self.0.sort()
    }
}

impl From<LabelExpression> for DnfLabelExpression {
    fn from(value: LabelExpression) -> Self {
        let dnf = value.dnf();
        let mut out = DnfLabelExpression::default();
        match dnf {
            LabelExpression::Or(lhs, rhs) => {
                out.extend(DnfLabelExpression::from(*lhs));
                out.extend(DnfLabelExpression::from(*rhs));
            }
            LabelExpression::And(lhs, rhs) => out.extend([
                LabelConjunct::try_from(*lhs).expect(
                    "Should never be possible, means we have an error in the DNF computation!",
                ),
                LabelConjunct::try_from(*rhs).expect(
                    "Should never be possible, means we have an error in the DNF computation!",
                ),
            ]),
            LabelExpression::Integer(val) => {
                out.append(LabelConjunct::Ap(val));
            }
            LabelExpression::Alias(_) => {
                panic!("This can only be used in unaliased LabelExpressions!")
            }
            LabelExpression::Boolean(val) => {
                out.append(LabelConjunct::Bool(val));
            }
            LabelExpression::Not(expr) => {
                out.append(LabelConjunct::Not(Box::new(
                    LabelConjunct::try_from(*expr)
                        .expect("Negation must only appear at atomics, erroro in DNF computation!"),
                )));
            }
        }
        out
    }
}

impl PartialOrd for LabelExpression {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LabelExpression {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        DnfLabelExpression::from(self.clone()).cmp(&DnfLabelExpression::from(other.clone()))
    }
}

impl Display for LabelConjunct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LabelConjunct::Bool(val) => write!(f, "{}", val),
            LabelConjunct::Ap(val) => write!(f, "{}", val),
            LabelConjunct::Not(val) => write!(f, "!{}", val),
        }
    }
}

impl Display for DnfLabelExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        if let Some(first) = iter.next() {
            write!(f, "{}", first)?;
        }
        for conjunct in iter {
            write!(f, " | {}", conjunct)?;
        }
        Ok(())
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
