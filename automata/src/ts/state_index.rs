pub trait StateIndex: Clone + Eq + std::hash::Hash + std::fmt::Debug {
    fn create(from: usize) -> Self;
}

impl<X: Clone + Eq + std::hash::Hash + std::fmt::Debug + From<usize>> StateIndex for X {
    fn create(from: usize) -> Self {
        from.into()
    }
}
