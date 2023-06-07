use automata::convert::ToDot;
use automata::DFA;
use yew::prelude::*;

fn build_dfa() -> DFA {
    DFA::from_parts_iters(
        [
            (0, 'a', 1),
            (0, 'b', 0),
            (1, 'a', 2),
            (1, 'b', 1),
            (2, 'a', 0),
            (2, 'b', 2),
        ],
        [1],
        0,
    )
}

fn print_dfa(dfa: DFA) -> Html {
    let js = dfa.to_dot().replace('"', "\x22");
    html! {
        <>
        <span id="dfa">{js}</span>
        </>
    }
}

#[function_component(App)]
fn app() -> Html {
    html! {
        <div>
        <h1>{ print_dfa(build_dfa()) }</h1>
        </div>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}
