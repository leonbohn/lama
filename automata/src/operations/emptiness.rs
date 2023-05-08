use crate::Acceptor;

pub trait EmptinessCheck: Acceptor {
    type Witness;

    fn is_empty(&self) -> bool;
    fn witness_nonemptiness(&self) -> Option<Self::Witness>;
}
