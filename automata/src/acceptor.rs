use crate::{ts::TransitionSystem, words::Word};

pub trait Acceptor<W: Word> {
    type TS: TransitionSystem;

    fn accepts(&self, word: &W) -> bool;
}
