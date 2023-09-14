use impl_tools::autoimpl;
use owo_colors::OwoColorize;

use automata::{
    alphabet::Simple,
    ts::{dag::Dag, FiniteState, Sproutable, ToDot},
    Alphabet, Class, Color, MealyMachine, RightCongruence, Set, TransitionSystem,
};

/// A priority mapping is essentially a [`crate::MealyMachine`], i.e. it reads
/// finite words and ouptuts a priority (which in this case is a `usize`).
pub type PriorityMapping<A = Simple> = RightCongruence<A, (), usize>;

#[derive(Clone, Copy, Ord, PartialEq, PartialOrd, Eq, Hash, Default)]
pub struct Annotation {
    pub(super) idempotent: bool,
    pub(super) good: Option<bool>,
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
    pub fn new(idempotent: bool, good: Option<bool>) -> Self {
        Self { idempotent, good }
    }
}

pub struct AnnotatedCongruence<A: Alphabet = Simple>(RightCongruence<A, Annotation, ()>);

#[autoimpl(for<T: trait + ?Sized> &T)]
pub trait ClassifiesIdempotents<A: Alphabet> {
    fn classify(&self, class: &Class<A::Symbol>) -> Option<bool>;
}

impl<A> ToDot for AnnotatedCongruence<A>
where
    A: Alphabet,
    A::Symbol: std::fmt::Display,
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
    pub fn canonic_coloring(
        &self,
    ) -> impl FiniteState
           + TransitionSystem<
        StateIndex = usize,
        StateColor = automata::congruence::ColoredClass<A::Symbol, Annotation>,
        Alphabet = A,
        EdgeColor = usize,
    > + Clone
           + '_ {
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
            println!("{:?}\n{:?}", seen, dag);
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

        (&self.0).map_edges(move |p, e, c, q| {
            let scc = tjdag.get(p).expect("Must be in an SCC");
            let info = dag.color(scc).expect("Must have worked on that SCC");

            info.expect("Every SCC must have a color")
        })
    }

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
                .collect(),
        )
    }
}

/// A family of weak priority mappings (FWPM) is a pair (C, M) where C is a
/// right congruence relation and for each class c of C, M_c is a Mealy machine.
/// Each mealy machine M_c is called a component of the FWPM and the mapping
/// it computes (on non-empty words) is weak in the sense that M_c(xy) <= M_c(x)
/// for all x and y.
#[derive(Debug, Clone)]
pub struct FWPM<A: Alphabet = Simple> {
    cong: RightCongruence<A>,
    pms: Vec<PriorityMapping<A>>,
}

impl<A: Alphabet> FWPM<A> {}
