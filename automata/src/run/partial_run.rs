use crate::{
    ts::Path,
    words::{Length, Repr},
    State, Symbol,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PartialRun<'a, Q, S: Symbol, L: Length> {
    path: Path<Q, S>,
    missing: S,
    repr: Repr<'a, S, L>,
}

impl<'a, Q: State, S: Symbol, L: Length> PartialRun<'a, Q, S, L> {
    pub fn new<R>(path: Path<Q, S>, missing: S, repr: R) -> Self
    where
        R: Into<Repr<'a, S, L>>,
    {
        Self {
            path,
            missing,
            repr: repr.into(),
        }
    }

    /// Computes the length of the successful computation.
    pub fn len(&self) -> usize {
        self.path.len()
    }

    /// Returns true if and only if the length of the successful computation
    /// is zero, i.e. no transition was taken.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn successful_path(&self) -> &Path<Q, S> {
        &self.path
    }

    pub fn missing_symbol(&self) -> &S {
        &self.missing
    }

    pub fn input_representation(&self) -> &Repr<'a, S, L> {
        &self.repr
    }
}
