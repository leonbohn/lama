use std::collections::{HashMap, HashSet};

use crate::{Id, LabelExpression};

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
enum Val {
    True,
    False,
    Undecided,
}

impl std::ops::BitOr for Val {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Val::True, _) | (_, Val::True) => Val::True,
            (Val::False, Val::False) => Val::False,
            _ => Val::Undecided,
        }
    }
}

impl std::ops::BitAnd for Val {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Val::False, _) | (_, Val::False) => Val::False,
            (Val::True, Val::True) => Val::True,
            _ => Val::Undecided,
        }
    }
}

impl std::ops::Not for Val {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Val::True => Val::False,
            Val::False => Val::True,
            Val::Undecided => Val::Undecided,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Interpretation {
    default: Val,
    map: HashMap<Id, Val>,
}

impl Interpretation {
    pub(crate) fn top() -> Self {
        Interpretation {
            default: Val::True,
            pos: HashSet::new(),
            neg: HashSet::new(),
        }
    }

    pub(crate) fn bot() -> Self {
        Interpretation {
            default: Val::False,
            pos: HashSet::new(),
            neg: HashSet::new(),
        }
    }

    pub(crate) fn just(id: Id) -> Self {
        Interpretation {
            default: Val::Undecided,
            pos: vec![id].into_iter().collect(),
            neg: HashSet::new(),
        }
    }
}

impl std::ops::BitOr for Interpretation {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Interpretation {
            pos: self.pos.intersection(&rhs.pos).cloned().collect(),
            neg: self.neg.union(&rhs.neg).cloned().collect(),
            default: self.default | rhs.default,
        }
    }
}

impl std::ops::BitAnd for Interpretation {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Interpretation {
            pos: self.pos.union(&rhs.pos).cloned().collect(),
            neg: self.neg.intersection(&rhs.neg).cloned().collect(),
            default: self.default & rhs.default,
        }
    }
}

impl std::ops::Not for Interpretation {
    type Output = Self;

    fn not(self) -> Self::Output {
        Interpretation {
            pos: self.neg,
            neg: self.pos,
            default: !self.default,
        }
    }
}

impl From<&LabelExpression> for Interpretation {
    fn from(expression: &LabelExpression) -> Self {
        match expression {
            LabelExpression::Boolean(b) => {
                if *b {
                    Interpretation::top()
                } else {
                    Interpretation::bot()
                }
            }
            LabelExpression::Integer(n) => Interpretation::just(*n),
            LabelExpression::Alias(_) => unimplemented!(),
            LabelExpression::Not(subexpr) => !Interpretation::from(subexpr.as_ref()),
            LabelExpression::And(l, r) => {
                Interpretation::from(l.as_ref()) & Interpretation::from(r.as_ref())
            }
            LabelExpression::Or(l, r) => {
                Interpretation::from(l.as_ref()) | Interpretation::from(r.as_ref())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::LabelExpression;

    use super::Interpretation;

    #[test]
    fn label_expr_to_interpretation() {
        let expr = LabelExpression::And(
            Box::new(LabelExpression::Or(
                Box::new(LabelExpression::Integer(1)),
                Box::new(LabelExpression::Boolean(false)),
            )),
            Box::new(LabelExpression::Integer(2)),
        );
        let interpretation = Interpretation::from(&expr);
        println!("{:?}", interpretation);
    }
}
