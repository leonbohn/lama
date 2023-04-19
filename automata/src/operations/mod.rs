use crate::Equivalent;
pub use crate::Transducer;

pub use self::product::Product;

mod product;
mod union;

impl<T: Transducer> Equivalent for T {
    fn equivalent(&self, other: &Self) -> bool {
        todo!()
    }
}
