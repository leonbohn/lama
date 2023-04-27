use crate::{ts::Visitor, Combined, Set, Successor};

impl<TS: Successor, Acc> Combined<TS, Acc> {
    pub fn trim_in_place(&mut self) {
        let mut bfs = self.bfs().iter().collect::<Set<_>>();
        self.ts_mut().states.retain(|s| bfs.contains(s));
    }

    pub fn trim(&self) -> Self
    where
        Self: Clone,
    {
        let mut out = self.clone();
        out.trim_in_place();
        out
    }
}
