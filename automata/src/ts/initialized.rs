use impl_tools::autoimpl;

use crate::{Pointed, TransitionSystem};

/// Holds an initialized transition system. Usually obtained through [`TransitionSystem::start()`].
#[autoimpl(Deref, DerefMut using self.ts)]
pub struct Initialized<TS: TransitionSystem> {
    pub(crate) ts: TS,
    pub(crate) start: TS::State,
}

impl<TS: TransitionSystem> Initialized<TS> {}

impl<TS: TransitionSystem> Pointed for Initialized<TS> {
    fn initial(&self) -> Self::State {
        self.start.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::{run::Evaluate, tests::simple_ts, Pointed, TransitionSystem};

    #[test]
    fn initialize_it() {
        let ts = simple_ts().start(0);
        assert_eq!(ts.run(&"abba".to_string()).evaluate(), Ok(0));
    }
}
