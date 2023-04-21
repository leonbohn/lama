use crate::Equivalent;
pub use crate::Transducer;

mod product;
mod union;

pub use product::DirectProduct;

impl<T: Transducer> Equivalent for T {
    fn equivalent(&self, other: &Self) -> bool {
        todo!()
    }
}
