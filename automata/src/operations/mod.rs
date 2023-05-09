use std::borrow::Borrow;

pub use crate::TransitionOutput;
use crate::{
    ts::{IntoParts, IntoTransitions},
    Acceptor, Equivalent, Pair, State, Successor, Symbol, DFA,
};

mod chain;

mod union;
pub use union::Union;

mod intersection;
pub use intersection::Intersection;

mod emptiness;
pub use emptiness::IsEmpty;

mod negation;
pub use negation::Negation;

mod product;
pub(crate) use product::product_transitions;
pub use product::Product;

mod trimming;

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::{
        operations::{Intersection, Negation, Union},
        Acceptor, DFA,
    };

    #[test]
    #[traced_test]
    fn dfa_operations() {
        let left = DFA::from_parts_iters(
            [
                (0, 'a', 1),
                (1, 'a', 2),
                (2, 'a', 0),
                (0, 'b', 0),
                (1, 'b', 1),
                (2, 'b', 2),
            ],
            [0],
            0,
        );
        let right =
            DFA::from_parts_iters([(0, 'a', 0), (1, 'a', 1), (0, 'b', 1), (1, 'b', 0)], [0], 0);

        let union = left.union(&right);

        println!(
            "{}\n{}\n==============================\n{:}",
            left, right, union
        );

        for p in ["aaa", "bb", "abb", "b"] {
            assert!(union.accepts(&p.into()), "Should accept {}", p);
        }
        for n in ["ab", "aba", "baa", "aababba"] {
            assert!(!union.accepts(&n.into()), "Should reject {}", n);
        }

        let intersection = left.intersection(&right);
        for p in ["", "aaabb", "bb", "aaa"] {
            assert!(intersection.accepts(&p.into()));
        }
        for n in ["a", "b", "aba", "bba", "aaab", "baabab"] {
            assert!(!intersection.accepts(&n.into()));
        }

        let negation = left.complement();
        for p in ["a", "aa", "ba"] {
            assert!(negation.accepts(&p.into()));
        }
        for n in ["", "aaa"] {
            assert!(!negation.accepts(&n.into()));
        }
    }
}
