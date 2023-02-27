/// A trait for the state index type. Implementors must be comparable, hashable, clonable and debuggable. The `create` method is used to create a new state index from a `u32`.
pub trait StateIndex: Clone + Eq + std::hash::Hash + std::fmt::Debug {
    /// Create a new state index from a `u32`.
    fn create(from: u32) -> Self;
}

impl<X: Clone + Eq + std::hash::Hash + std::fmt::Debug + From<u32>> StateIndex for X {
    fn create(from: u32) -> Self {
        from.into()
    }
}
