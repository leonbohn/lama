use crate::{ts::TransitionSystem, words::Word};

pub struct PartialRun<'ts, 'w, W: Word, TS: TransitionSystem> {
    pub(crate) word: &'w W,
    pub(crate) ts: &'ts TS,
    pub(crate) state: Option<TS::Q>,
    pub(crate) position: usize,
    pub(crate) seq: Vec<TS::Q>,
}
