pub trait StateIndex: Clone + Eq + std::hash::Hash + std::fmt::Debug {
    fn create(from: u32) -> Self;
}

impl<X: Clone + Eq + std::hash::Hash + std::fmt::Debug + From<u32>> StateIndex for X {
    fn create(from: u32) -> Self {
        from.into()
    }
}
