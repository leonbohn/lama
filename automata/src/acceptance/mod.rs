use std::collections::BTreeSet;

use crate::{mapping::Mapping, ts::StateIndex};

pub trait Induced: Eq {}
impl<T: Eq> Induced for T {}

pub trait Acceptance {
    type Over: Induced;

    fn is_accepting(&self, state: Self::Over) -> bool;
}

pub trait HasAcceptance {
    type Acceptance: Acceptance;
    fn acceptance(&self) -> &Self::Acceptance;
}

pub enum ParityKind {
    Even,
    Odd,
}

impl From<ParityKind> for bool {
    fn from(parity: ParityKind) -> Self {
        match parity {
            ParityKind::Even => false,
            ParityKind::Odd => true,
        }
    }
}

impl From<ParityKind> for u8 {
    fn from(parity: ParityKind) -> Self {
        match parity {
            ParityKind::Even => 0,
            ParityKind::Odd => 1,
        }
    }
}

pub trait Parity {
    fn parity(&self) -> ParityKind;
}

impl<T> Parity for T
where
    T: Copy + std::ops::Rem<u8>,
    T::Output: PartialEq<u8>,
{
    fn parity(&self) -> ParityKind {
        if *self % 2 == 0 {
            ParityKind::Even
        } else {
            ParityKind::Odd
        }
    }
}

pub struct ReachabilityCondition<M>(M);

impl<T, M> Acceptance for ReachabilityCondition<M>
where
    T: Parity,
    M: Mapping<Output = T>,
    M::Input: Induced,
{
    type Over = M::Input;

    fn is_accepting(&self, state: Self::Over) -> bool {
        self.0.apply(state).parity().into()
    }
}
