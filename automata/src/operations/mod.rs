use crate::Equivalent;
pub use crate::WithOutput;

// mod product;
mod union;

// pub use product::DirectProduct;

impl<T: WithOutput> Equivalent for T {
    fn equivalent(&self, _other: &Self) -> bool {
        todo!()
    }
}
