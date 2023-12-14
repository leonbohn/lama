use std::fmt::{Debug, Display, Write};

use itertools::Itertools;

use crate::{
    alphabet::{Directional, InvertibleChar},
    automaton::{IntoDPA, WithInitial},
    congruence::{ColoredClass, FORC},
    prelude::{
        DPALike, IntoMealyMachine, IntoMooreMachine, MealyLike, MooreLike, Simple, Symbol, SymbolOf,
    },
    Alphabet, Class, Color, Map, Pointed, RightCongruence, Show, TransitionSystem,
};

use super::{
    transition_system::{Indexes, IsTransition},
    Deterministic, IndexType, BTS,
};

pub trait Dottable: TransitionSystem {
    /// Compute the graphviz representation, for more information on the DOT format,
    /// see the [graphviz documentation](https://graphviz.org/doc/info/lang.html).
    fn dot_representation(&self) -> String {
        let header = std::iter::once(format!(
            "digraph {} {{",
            self.dot_name().unwrap_or("A".to_string())
        ))
        .chain(self.dot_header_statements());

        let states = self.state_indices().map(|q| {
            format!(
                "{} [{}]",
                self.dot_state_ident(q),
                self.dot_state_attributes(q)
                    .into_iter()
                    .map(|attr| attr.to_string())
                    .join(", ")
            )
        });

        let transitions = self.state_indices().flat_map(|q| {
            self.edges_from(q)
                .expect("edges_from may not return none for state that exists")
                .map(move |t| {
                    format!(
                        "{} -> {} [{}]",
                        self.dot_state_ident(q),
                        self.dot_state_ident(t.target()),
                        self.dot_transition_attributes(t)
                            .into_iter()
                            .map(|attr| attr.to_string())
                            .join(", ")
                    )
                })
        });

        let mut lines = header
            .chain(states)
            .chain(transitions)
            .chain(std::iter::once("}".to_string()));
        lines.join("\n")
    }

    fn dot_header_statements(&self) -> impl IntoIterator<Item = String> {
        []
    }

    fn dot_name(&self) -> Option<String>;

    fn dot_transition_attributes<'a>(
        &'a self,
        t: Self::TransitionRef<'a>,
    ) -> impl IntoIterator<Item = DotTransitionAttribute> {
        []
    }
    fn dot_state_ident(&self, idx: Self::StateIndex) -> String;
    fn dot_state_attributes(
        &self,
        idx: Self::StateIndex,
    ) -> impl IntoIterator<Item = DotStateAttribute> {
        []
    }
    /// Renders the object visually (as PNG) and returns a vec of bytes/u8s encoding
    /// the rendered image. This method is only available on the `graphviz` crate feature
    /// and makes use of temporary files.
    #[cfg(feature = "graphviz")]
    fn render(&self) -> Result<Vec<u8>, std::io::Error> {
        use std::io::{Read, Write};

        use tracing::trace;
        let dot = self.dot_representation();
        trace!("writing dot representation\n{}", dot);

        let mut child = std::process::Command::new("dot")
            .arg("-Tpng")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()?;

        if let Some(mut stdin) = child.stdin.take() {
            stdin.write_all(dot.as_bytes())?;
        }

        let mut output = Vec::new();
        if let Some(mut stdout) = child.stdout.take() {
            stdout.read_to_end(&mut output)?;
        }

        let status = child.wait()?;
        if !status.success() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("dot process exited with status: {}", status),
            ));
        }

        Ok(output)
    }

    /// Attempts to render the object to a file with the given filename. This method
    /// is only available on the `graphviz` crate feature and makes use of temporary files.
    #[cfg(feature = "graphviz")]
    fn render_to_file_name(&self, filename: &str) -> Result<(), std::io::Error> {
        use std::io::{Read, Write};
        use tracing::trace;

        trace!("Outputting dot and rendering to png");
        let dot = self.dot_representation();
        let mut tempfile = tempfile::NamedTempFile::new()?;

        tempfile.write_all(dot.as_bytes())?;
        let tempfile_name = tempfile.path();

        let mut child = std::process::Command::new("dot")
            .arg("-Tpng")
            .arg("-o")
            .arg(filename)
            .arg(tempfile_name)
            .spawn()?;
        if !child.wait()?.success() {
            Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                child
                    .stdout
                    .map_or("Error in dot...".to_string(), |mut err| {
                        let mut buf = String::new();
                        err.read_to_string(&mut buf);
                        buf
                    }),
            ))
        } else {
            Ok(())
        }
    }

    /// First creates a rendered PNG using [`Self::render_tempfile()`], after which the rendered
    /// image is displayed using a locally installed image viewer (`eog` on linux, `qlmanage`
    /// i.e. quicklook on macos and nothing yet on windows).
    #[cfg(feature = "graphviz")]
    fn display_rendered(&self) -> Result<(), std::io::Error> {
        display_png(self.render()?)?;
        std::thread::sleep(std::time::Duration::from_secs(1));
        Ok(())
    }
}

impl<A: Alphabet> Dottable for crate::DFA<A> {
    fn dot_name(&self) -> Option<String> {
        Some("DFA".into())
    }

    fn dot_state_ident(&self, idx: Self::StateIndex) -> String {
        format!("q{}", idx.show())
    }

    fn dot_transition_attributes<'a>(
        &'a self,
        t: Self::TransitionRef<'a>,
    ) -> impl IntoIterator<Item = DotTransitionAttribute> {
        [DotTransitionAttribute::Label(t.expression.show())].into_iter()
    }

    fn dot_state_attributes(
        &self,
        idx: Self::StateIndex,
    ) -> impl IntoIterator<Item = DotStateAttribute> {
        let shape = if self.state_color(idx).unwrap() {
            "doublecircle"
        } else {
            "circle"
        };
        vec![
            DotStateAttribute::Shape(shape.into()),
            DotStateAttribute::Label(self.dot_state_ident(idx)),
        ]
    }
}
impl<A: Alphabet, Q: Color, C: Color> Dottable for crate::RightCongruence<A, Q, C> {
    fn dot_name(&self) -> Option<String> {
        Some("Congruence".into())
    }

    fn dot_state_ident(&self, idx: Self::StateIndex) -> String {
        format!("c{}", idx.show())
    }

    fn dot_transition_attributes<'a>(
        &'a self,
        t: Self::TransitionRef<'a>,
    ) -> impl IntoIterator<Item = DotTransitionAttribute> {
        [DotTransitionAttribute::Label(format!(
            "{}|{}",
            t.expression.show(),
            t.color.show()
        ))]
        .into_iter()
    }

    fn dot_state_attributes(
        &self,
        idx: Self::StateIndex,
    ) -> impl IntoIterator<Item = DotStateAttribute> {
        vec![DotStateAttribute::Label(format!(
            "{}|{}",
            self.dot_state_ident(idx),
            self.state_color(idx).unwrap().show()
        ))]
    }
}

impl<M: MooreLike> Dottable for IntoMooreMachine<M> {
    fn dot_name(&self) -> Option<String> {
        Some("DPA".into())
    }

    fn dot_state_attributes(
        &self,
        idx: Self::StateIndex,
    ) -> impl IntoIterator<Item = DotStateAttribute> {
        let color = self
            .state_color(idx)
            .map(|c| format!(" | {}", c.show()))
            .unwrap_or("".to_string());
        vec![DotStateAttribute::Label(format!(
            "{}{color}",
            self.dot_state_ident(idx)
        ))]
    }

    fn dot_transition_attributes<'a>(
        &'a self,
        t: Self::TransitionRef<'a>,
    ) -> impl IntoIterator<Item = DotTransitionAttribute> {
        vec![DotTransitionAttribute::Label(format!(
            "{}|{}",
            t.expression().show(),
            t.color().show()
        ))]
    }

    fn dot_state_ident(&self, idx: Self::StateIndex) -> String {
        format!("q{}", idx.show())
    }
}

impl<M: MealyLike> Dottable for IntoMealyMachine<M> {
    fn dot_name(&self) -> Option<String> {
        Some("DPA".into())
    }

    fn dot_state_attributes(
        &self,
        idx: Self::StateIndex,
    ) -> impl IntoIterator<Item = DotStateAttribute> {
        let color = self
            .state_color(idx)
            .map(|c| format!(" | {}", c.show()))
            .unwrap_or("".to_string());
        vec![DotStateAttribute::Label(format!(
            "{}{color}",
            self.dot_state_ident(idx)
        ))]
    }

    fn dot_transition_attributes<'a>(
        &'a self,
        t: Self::TransitionRef<'a>,
    ) -> impl IntoIterator<Item = DotTransitionAttribute> {
        vec![DotTransitionAttribute::Label(format!(
            "{}|{}",
            t.expression().show(),
            t.color().show()
        ))]
    }

    fn dot_state_ident(&self, idx: Self::StateIndex) -> String {
        format!("q{}", idx.show())
    }
}

impl<D: DPALike> Dottable for IntoDPA<D> {
    fn dot_name(&self) -> Option<String> {
        Some("DPA".into())
    }

    fn dot_state_attributes(
        &self,
        idx: Self::StateIndex,
    ) -> impl IntoIterator<Item = DotStateAttribute> {
        let color = self
            .state_color(idx)
            .map(|c| format!(" | {}", c.show()))
            .unwrap_or("".to_string());
        vec![DotStateAttribute::Label(format!(
            "{}{color}",
            self.dot_state_ident(idx)
        ))]
    }

    fn dot_transition_attributes<'a>(
        &'a self,
        t: Self::TransitionRef<'a>,
    ) -> impl IntoIterator<Item = DotTransitionAttribute> {
        vec![DotTransitionAttribute::Label(format!(
            "{}|{}",
            t.expression().show(),
            t.color().show()
        ))]
    }

    fn dot_state_ident(&self, idx: Self::StateIndex) -> String {
        format!("q{}", idx.show())
    }
}

/// Enum that abstracts attributes in the DOT format.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DotStateAttribute {
    /// The label of a node
    Label(String),
    /// The shape of a node
    Shape(String),
    /// The color of a node
    Color(String),
}

impl Display for DotStateAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DotStateAttribute::Label(s) => format!("label=\"{}\"", s),
                DotStateAttribute::Shape(s) => format!("shape=\"{}\"", s),
                DotStateAttribute::Color(c) => format!("color=\"{}\"", c),
            }
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DotTransitionAttribute {
    Label(String),
}

impl Display for DotTransitionAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DotTransitionAttribute::Label(lbl) => write!(f, "label=\"{lbl}\""),
        }
    }
}

// impl<A: Alphabet, Q: Color + Debug, C: Color + Debug> ToDot for Vec<RightCongruence<A, Q, C>>
// where
//     A::Symbol: Display,
//     Q: DotStateColorize,
//     DotTransitionInfo<C, A>: DotTransition,
// {
//     fn dot_representation(&self) -> String {
//         format!("digraph A {{\n{}\n{}\n}}\n", self.header(), self.body(""),)
//     }

//     fn header(&self) -> String {
//         [
//             "compound=true".to_string(),
//             "fontname=\"Helvetica,Arial,sans-serif\"\nrankdir=LR".to_string(),
//             "init [label=\"\", shape=none]".into(),
//             "node [shape=rect]".into(),
//         ]
//         .join("\n")
//     }

//     fn body(&self, _prefix: &str) -> String {
//         self.iter()
//             .enumerate()
//             .map(|(i, cong)| {
//                 format!(
//                     "subgraph cluster_{} {{\n{}\n{}\n}}\n",
//                     i,
//                     cong.header(),
//                     cong.body(&format!("{i}"))
//                 )
//             })
//             .join("\n")
//     }
// }

// impl<A: Alphabet, Q: Color + Debug, C: Color + Debug> ToDot for FORC<A, Q, C>
// where
//     A::Symbol: Display,
//     Q: DotStateColorize,
//     DotTransitionInfo<C, A>: DotTransition,
// {
//     fn dot_representation(&self) -> String {
//         format!("digraph A {{\n{}\n{}\n}}\n", self.header(), self.body(""),)
//     }

//     fn header(&self) -> String {
//         [
//             "compund=true".to_string(),
//             "fontname=\"Helvetica,Arial,sans-serif\"\nrankdir=LR".to_string(),
//             "init [label=\"\", shape=none]".into(),
//             "node [shape=rect]".into(),
//         ]
//         .join("\n")
//     }

//     fn body(&self, _prefix: &str) -> String {
//         let mut lines = self
//             .progress
//             .iter()
//             .map(|(class, prc)| {
//                 format!(
//                     "subgraph cluster_{} {{\n{}\n{}\n}}\n",
//                     self.leading()
//                         .state_color(*class)
//                         .unwrap()
//                         .class()
//                         .mr_to_string(),
//                     prc.header(),
//                     prc.body(&class.to_string())
//                 )
//             })
//             .collect_vec();

//         lines.push("init [label=\"\", shape=none]".to_string());
//         let eps_prc = self
//             .prc(&Class::epsilon())
//             .expect("Must have at least the epsilon prc");
//         lines.push(format!(
//             "init -> \"{},init\" [style=\"solid\"]",
//             eps_prc
//                 .state_color(eps_prc.initial())
//                 .expect("State should have a color")
//         ));

//         for state in self.leading.state_indices() {
//             for sym in self.leading.alphabet().universe() {
//                 if let Some(edge) = self.leading.transition(state, sym) {
//                     let _source_prc = self
//                         .prc(
//                             self.leading
//                                 .state_color(state)
//                                 .expect("State should be colored")
//                                 .class(),
//                         )
//                         .expect("Must have a prc for every state");
//                     let _target_prc = self
//                         .prc(
//                             self.leading
//                                 .state_color(edge.target())
//                                 .expect("State should be colored")
//                                 .class(),
//                         )
//                         .expect("Must have a prc for every state");
//                     lines.push(format!(
//                         "\"{},init\" -> \"{},init\" [label = \"{}\", style=\"dashed\", ltail=\"cluster_{}\", lhead=\"cluster_{}\"]",
//                         self.leading.state_color(state).expect("State should be colored"),
//                         self.leading.state_color(edge.target()).expect("State should be colored"),
//                         sym,
//                         self.leading.state_color(state).expect("State should be colored").class().mr_to_string(),
//                         self.leading.state_color(edge.target()).expect("State should be colored").class().mr_to_string()
//                     ));
//                 }
//             }
//         }

//         lines.join("\n")
//     }
// }

/// Renders the given dot string to a png file and displays it using the default
/// image viewer on the system.
#[cfg(feature = "graphviz")]
pub fn display_dot(dot: &str) -> Result<(), std::io::Error> {
    display_png(render_dot_to_tempfile(dot)?)
}

#[cfg(feature = "graphviz")]
fn render_dot_to_tempfile(dot: &str) -> Result<Vec<u8>, std::io::Error> {
    use std::{
        io::{Read, Write},
        process::Stdio,
    };

    let mut tempfile = tempfile::NamedTempFile::new()?;
    tempfile.write_all(dot.as_bytes())?;
    let tempfile_name = tempfile.path();

    let image_tempfile = tempfile::Builder::new().suffix(".png").tempfile()?;

    let mut child = std::process::Command::new("dot")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .arg("-Tpng")
        .spawn()?;

    let mut stdin = child.stdin.take().expect("Could not get handle to stdin");
    stdin.write_all(dot.as_bytes())?;

    match child.wait_with_output() {
        Ok(res) => {
            if res.status.success() {
                Ok(res.stdout)
            } else {
                let stderr_output =
                    String::from_utf8(res.stderr).expect("could not parse stderr of dot");
                tracing::error!("Could not render, dot reported\n{}", &stderr_output);
                Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    stderr_output,
                ))
            }
        }
        Err(e) => todo!(),
    }
}

#[cfg(target_os = "linux")]
fn start_linux(contents: Vec<u8>) {
    use std::{
        io::{Read, Stdin, Write},
        process::Stdio,
    };

    let mut child = std::process::Command::new("display")
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();
    let mut stdin = child.stdin.take().expect("Could not take stdin");
    std::thread::spawn(move || {
        stdin
            .write_all(&contents)
            .expect("Could not write file to stdin");
    });
}

#[cfg(target_os = "macos")]
fn start_macos(contents: Vec<u8>) {
    use std::{
        io::{Read, Stdin, Write},
        process::Stdio,
    };

    let mut child = std::process::Command::new("open")
        .arg("-a")
        .arg("Preview.app")
        .arg("-f")
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();
    let mut stdin = child.stdin.take().expect("Could not take stdin");
    std::thread::spawn(move || {
        stdin
            .write_all(&contents)
            .expect("Could not write file to stdin");
    });
}

#[cfg(feature = "graphviz")]
fn display_png(contents: Vec<u8>) -> Result<(), std::io::Error> {
    #[cfg(target_os = "linux")]
    start_linux(contents);
    #[cfg(target_os = "macos")]
    start_macos(contents);
    #[cfg(target_os = "windows")]
    std::process::Command::new("cmd")
        .arg("/c")
        .arg(format!("start {}", rendered_path.display()))
        .spawn()?;
    #[cfg(target_os = "windows")]
    std::thread::sleep(std::time::Duration::from_secs(2));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        alphabet,
        congruence::FORC,
        prelude::DPA,
        ts::{Sproutable, NTS},
        Class, Pointed, RightCongruence,
    };

    use super::Dottable;

    #[test_log::test]
    fn render_dfa() {
        let dfa = NTS::builder()
            .with_transitions([
                (0, 'a', (), 0),
                (0, 'b', (), 1),
                (1, 'a', (), 1),
                (1, 'b', (), 0),
            ])
            .with_colors([false, true])
            .into_dfa(0);
        dfa.display_rendered().unwrap();
    }

    #[test]
    #[ignore]
    fn display_forc() {
        let alphabet = alphabet!(simple 'a', 'b');
        let mut cong = RightCongruence::new(alphabet.clone());
        let q0 = cong.initial();
        let q1 = cong.add_state(vec!['a']);
        cong.add_edge(q0, 'a', q1, ());
        cong.add_edge(q0, 'b', q0, ());
        cong.add_edge(q1, 'a', q0, ());
        cong.add_edge(q1, 'b', q1, ());

        let mut prc_e = RightCongruence::new(alphabet.clone());
        let e0 = prc_e.initial();
        let e1 = prc_e.add_state(vec!['a']);
        let e2 = prc_e.add_state(vec!['b']);
        prc_e.add_edge(e0, 'a', e1, ());
        prc_e.add_edge(e0, 'b', e2, ());
        prc_e.add_edge(e1, 'a', e1, ());
        prc_e.add_edge(e1, 'b', e2, ());
        prc_e.add_edge(e2, 'a', e2, ());
        prc_e.add_edge(e2, 'b', e2, ());

        let mut prc_a = RightCongruence::new(alphabet);
        let a0 = prc_a.initial();
        let a1 = prc_a.add_state(vec!['a']);
        let a2 = prc_a.add_state(vec!['b']);
        let a3 = prc_a.add_state(vec!['a', 'a']);
        prc_a.add_edge(a0, 'a', a1, ());
        prc_a.add_edge(a0, 'b', a2, ());
        prc_a.add_edge(a1, 'a', a3, ());
        prc_a.add_edge(a1, 'b', a2, ());
        prc_a.add_edge(a2, 'a', a1, ());
        prc_a.add_edge(a2, 'b', a2, ());
        prc_a.add_edge(a3, 'a', a3, ());
        prc_a.add_edge(a3, 'b', a3, ());

        let forc = FORC::from_iter(cong, [(0, prc_e), (1, prc_a)].iter().cloned());
        todo!()
        // forc.render_to_file_name("/home/leon/test.png");
    }

    #[test]
    #[ignore]
    fn dot_render_and_display() {
        let alphabet = alphabet!(simple 'a', 'b');
        let mut cong = RightCongruence::new(alphabet);
        let q0 = cong.initial();
        let q1 = cong.add_state(vec!['a']);
        cong.add_edge(q0, 'a', q1, ());
        cong.add_edge(q0, 'b', q0, ());
        cong.add_edge(q1, 'a', q0, ());
        cong.add_edge(q1, 'b', q1, ());

        cong.display_rendered();
        let three_congs = vec![cong.clone(), cong.clone(), cong];
        todo!()
        // three_congs.display_rendered();
    }

    #[test]
    #[ignore]
    fn dot_render_dpa() {
        let alphabet = alphabet!(simple 'a', 'b');
        let mut dpa = DPA::new(alphabet, ());
        let q0 = dpa.initial();
        let q1 = dpa.add_state(());
        dpa.add_edge(q0, 'a', q0, 1);
        dpa.add_edge(q0, 'b', q1, 2);
        dpa.add_edge(q1, 'a', q1, 0);
        dpa.add_edge(q1, 'b', q0, 2);
        println!("{:?}", dpa);
        dpa.display_rendered();
    }
}
