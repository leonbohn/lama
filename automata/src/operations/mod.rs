use crate::Equivalent;
pub use crate::TransitionOutput;

// mod product;
mod union;

// pub use product::DirectProduct;

impl<T: TransitionOutput> Equivalent for T {
    fn equivalent(&self, _other: &Self) -> bool {
        todo!()
    }
}
