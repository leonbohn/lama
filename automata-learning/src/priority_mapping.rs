use std::fmt::Debug;

use impl_tools::autoimpl;
use owo_colors::OwoColorize;

use automata::{
    automata::MooreLike,
    prelude::*,
    ts::dot::{DotStateAttribute, DotStateColorize},
    Set,
};

/// A priority mapping is essentially a [`crate::MealyMachine`], i.e. it reads
/// finite words and ouptuts a priority (which in this case is a `usize`).
pub type PriorityMapping<A = Simple> = RightCongruence<A, (), usize>;

/// Stores information on classes/states of a [`RightCongruence`]. This may be
/// extended in the futuer, but for now it simply stores whether a class c is
/// idempoten (meaning cc ~ c) and whether it is good (in the sense that the
/// omega iteration of c is in the language L_c).
#[derive(Clone, Copy, Ord, PartialEq, PartialOrd, Eq, Hash, Default)]
pub struct Annotation {
    pub(super) idempotent: bool,
    pub(super) good: Option<bool>,
}

impl Show for Annotation {
    fn show(&self) -> String {
        if !self.idempotent {
            "#".into()
        } else {
            match self.good {
                Some(true) => "+".into(),
                Some(false) => "-".into(),
                None => "o".into(),
            }
        }
    }
}

impl DotStateColorize for Annotation {
    fn dot_state_colorize(&self, base: &mut automata::ts::dot::DotStateData) {
        let i = if self.idempotent { "*" } else { "" };
        base.push_attribute(DotStateAttribute::Label(format!(
            "{}{}",
            base.raw_name(),
            i,
        )));
        if let Some(b) = self.good {
            base.push_attribute(DotStateAttribute::Color(if b {
                "green".to_string()
            } else {
                "red".to_string()
            }));
        }
    }
}

impl std::fmt::Debug for Annotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(b) = self.good {
            write!(f, "{}", if b { '+' } else { '-' })?;
        }
        Ok(())
    }
}

impl Annotation {
    /// Creates a new annotation.
    pub fn new(idempotent: bool, good: Option<bool>) -> Self {
        Self { idempotent, good }
    }
}

/// This a simple newtype wrapper around a congruence, which has no edge colors and uses
/// [`Annotation`]s as state colors.
#[derive(Clone)]
pub struct AnnotatedCongruence<A: Alphabet = Simple>(RightCongruence<A, Annotation, ()>);

#[autoimpl(for<T: trait + ?Sized> &T)]
pub trait ClassifiesIdempotents<A: Alphabet> {
    fn classify(&self, class: &Class<A::Symbol>) -> Option<bool>;
}

impl<A: Alphabet> Debug for AnnotatedCongruence<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<A> ToDot for AnnotatedCongruence<A>
where
    A: Alphabet,
    RightCongruence<A, Annotation, ()>: ToDot,
{
    fn dot_representation(&self) -> String {
        format!("digraph A {{\n{}\n{}\n}}\n", self.header(), self.body(""))
    }

    fn header(&self) -> String {
        self.0.header()
    }

    fn body(&self, prefix: &str) -> String {
        self.0.body(prefix)
    }
}

impl<A: Alphabet> AnnotatedCongruence<A> {
    /// Computes the canonic coloring on a given annotated congruence. This makes use
    /// of the dag of strongly connected components of the congruence. For more information
    /// on how the computation is done exactly, see [Section 5, Step 2](https://arxiv.org/pdf/2302.11043.pdf).
    pub fn canonic_coloring(&self) -> MooreMachine<A, usize> {
        // we first need to decompose into sccs and mark them with the color of the
        // idempotent that it contains.
        let tjdag = self.0.tarjan_dag();
        let mut dag: Dag<Result<usize, Option<bool>>> =
            tjdag.fold_state_colors(Err(None), |acc, x| match (acc, x.color().good) {
                (Err(None), None) => Err(None),
                (Err(Some(x)), Some(y)) => Err(Some(x || y)),
                (Err(Some(x)), None) => Err(Some(x)),
                (Err(None), Some(x)) => Err(Some(x)),
                (Ok(_), _) => unreachable!(),
            });

        let mut seen = Set::default();
        'outer: loop {
            if dag.masked_is_empty(&seen) {
                break 'outer;
            }
            let t = dag.masked_terminal_nodes(&seen).next().unwrap();
            assert!(seen.insert(t), "This must not have been seen before!");
            let i = dag
                .immediate(t)
                .map(|i| {
                    dag.color(i)
                        .expect("This node must exist")
                        .expect("The color of this node must already be known")
                })
                .max()
                .unwrap_or(0);
            let parity = i % 2 == 0;

            let offset = match dag.color(t).expect("We know this node exists") {
                Err(Some(true)) => !parity as usize,
                Err(Some(false)) => parity as usize,
                Err(None) => 0,
                Ok(_) => unreachable!(),
            };
            *dag.color_mut(t).expect("This node exists") = Ok(i + offset);
        }

        (&self.0)
            .erase_edge_colors()
            .map_state_colors(move |q| {
                let scc = tjdag.get(q).expect("Must be in an SCC");
                let info = dag.color(scc).expect("Must have worked on that SCC");

                info.expect("Every SCC must have a color")
            })
            .collect_with_initial()
    }

    /// Takes a reference to a right congruence and a function that classifies idempotents
    /// of the congruence to construct an annotated congruence.
    pub fn build<Q, C, F>(rc: &RightCongruence<A, Q, C>, f: F) -> Self
    where
        Q: Color,
        C: Color,
        F: ClassifiesIdempotents<A>,
    {
        Self(
            rc.erase_edge_colors()
                .map_state_colors(|c| {
                    let cls = c.class();
                    if !cls.is_empty() && rc.is_idempotent(cls) {
                        let b = f.classify(cls);
                        c.recolor(Annotation::new(true, b))
                    } else {
                        c.recolor(Annotation::new(false, None))
                    }
                })
                .collect_with_initial(),
        )
    }
}

/// A family of weak priority mappings (FWPM) is a pair (C, M) where C is a
/// right congruence relation and for each class c of C, M_c is a Mealy machine.
/// Each mealy machine M_c is called a component of the FWPM and the mapping
/// it computes (on non-empty words) is weak in the sense that M_c(xy) <= M_c(x)
/// for all x and y.
#[derive(Debug, Clone)]
pub struct Fwpm<A: Alphabet = Simple> {
    cong: RightCongruence<A>,
    pms: Vec<PriorityMapping<A>>,
}

impl<A: Alphabet> Fwpm<A> {}
