use std::fmt::Debug;

use itertools::Itertools;

use crate::prelude::*;

#[derive(Clone)]
pub struct Normalized<S: Symbol> {
    upw: Reduced<S>,
    pre_loop_count: usize,
    loop_size: usize,
}

impl<S: Symbol> Normalized<S> {
    pub fn new(upw: Reduced<S>, pre_loop_count: usize, loop_size: usize) -> Self {
        assert!(loop_size > 0, "loop size must be positive");
        Self {
            upw,
            pre_loop_count,
            loop_size,
        }
    }

    pub fn base_iter(&self) -> impl Iterator<Item = S> + '_ {
        self.upw.spoke().symbols().chain(
            std::iter::repeat(self.upw.cycle())
                .take(self.pre_loop_count)
                .flat_map(|cycle| cycle.symbols()),
        )
    }

    pub fn rec_iter(&self) -> impl Iterator<Item = S> + '_ {
        std::iter::repeat(self.upw.cycle())
            .take(self.loop_size)
            .flat_map(|cycle| cycle.symbols())
    }
}

impl<S: Symbol> Debug for Normalized<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let base = self.base_iter().map(|s| s.show()).join("");
        let rec = self.rec_iter().map(|s| s.show()).join("");
        write!(f, "{base}·{rec}~{base}")
    }
}

#[cfg(test)]
mod tests {
    use crate::{upw, word::normalized::Normalized};

    #[test]
    fn debug_normalized() {
        let upw = upw!("ab", "c");
        let nupw = Normalized::new(upw, 1, 2);
        assert_eq!(format!("{:?}", nupw), "abc·cc~abc");
    }
}
