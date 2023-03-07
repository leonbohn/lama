use automata::{Acceptor, Dfa, Growable, Pointed};

fn main() {
    let mut dfa = Dfa::trivial();
    let q0 = dfa.initial();
    let q1 = 1;
    assert!(dfa.add_state(&q1));
    dfa.add_transition(&q0, 'a', &q0);
    dfa.add_transition(&q1, 'a', &q1);
    dfa.add_transition(&q0, 'b', &q1);
    dfa.add_transition(&q1, 'b', &q0);
    dfa.acceptance_mut().set_accepting(q0);
    assert!(dfa.accepts(&"b"));
}
