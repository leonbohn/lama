use crate::{acceptance::HasAcceptance, ts::Congruence};

pub trait Automaton: Congruence + HasAcceptance {}
impl<Aut: Congruence + HasAcceptance> Automaton for Aut {}
