use hoars::HoaAutomaton;

use crate::{automaton::IntoDPA, prelude::*};

use super::HoaAlphabet;

pub trait ToHoa: TransitionSystem + Pointed {
    fn to_hoa(&self) -> String {
        hoars::output::to_hoa(&self.to_hoa_automaton())
    }

    fn to_hoa_automaton(&self) -> HoaAutomaton;
}

impl<D: DPALike<Alphabet = HoaAlphabet>> ToHoa for IntoDPA<D> {
    fn to_hoa_automaton(&self) -> HoaAutomaton {
        let mut aut = HoaAutomaton::default();
        aut.add_header_item(hoa_alphabet_to_header_item(self.alphabet()));
        todo!()
    }
}

fn hoa_alphabet_to_header_item(alphabet: &HoaAlphabet) -> hoars::HeaderItem {
    todo!()
}
