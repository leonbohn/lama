mod map;
pub use map::*;

mod product;
pub use product::*;

mod restricted;
pub use restricted::*;

mod subset;
pub use subset::SubsetConstruction;

mod reverse;
pub use reverse::Reversed;

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{prelude::*, ts::finite::ReachedColor};

    #[test]
    fn product() {
        let mut dfa = crate::DFA::new_for_alphabet(CharAlphabet::new(['a', 'b']));
        let s0 = dfa.add_state(true);
        let s1 = dfa.add_state(false);
        let _e0 = dfa.add_edge(s0, 'a', s1, Void);
        let _e1 = dfa.add_edge(s0, 'b', s0, Void);
        let _e2 = dfa.add_edge(s1, 'a', s1, Void);
        let _e3 = dfa.add_edge(s1, 'b', s0, Void);

        let mut dfb = crate::DFA::new_for_alphabet(CharAlphabet::new(['a', 'b']));
        let s0 = dfb.add_state(true);
        let s1 = dfb.add_state(true);
        let _e0 = dfb.add_edge(s0, 'a', s1, Void);
        let _e1 = dfb.add_edge(s0, 'b', s0, Void);
        let _e2 = dfb.add_edge(s1, 'a', s1, Void);
        let _e3 = dfb.add_edge(s1, 'b', s0, Void);

        let xxx = dfa.ts_product(dfb);
        assert_eq!(xxx.reached_state_index("abb"), Some(ProductIndex(0, 0)));
        assert_eq!(xxx.reached_state_color("aa"), Some((false, true)));

        let yyy = xxx.clone().map_state_colors(|(a, b)| a || b).into_dfa();
        assert_eq!(yyy.reached_state_color("aa"), Some(true));
    }
}
