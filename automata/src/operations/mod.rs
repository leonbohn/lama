use crate::Equivalent;
pub use crate::TransitionOutput;

mod mapping;
mod product;

impl<T: TransitionOutput> Equivalent for T {
    fn equivalent(&self, _other: &Self) -> bool {
        todo!()
    }
}
