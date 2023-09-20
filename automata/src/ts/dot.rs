use std::fmt::{Debug, Display};

use itertools::Itertools;

use crate::{
    automata::WithInitial,
    congruence::{ColoredClass, FORC},
    prelude::{Symbol, SymbolOf},
    ts::FiniteState,
    Alphabet, Class, Color, Map, Pointed, RightCongruence, TransitionSystem,
};

use super::{
    transition_system::{Indexes, IsTransition},
    IndexType, BTS,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DotStateInfo<Idx: IndexType, Q: Color> {
    idx: Idx,
    color: Q,
    prefix: String,
}

pub trait DotState {
    fn dot_statement(&self) -> String {
        format!("node [{}]\n{}", self.attributes(), self.name())
    }
    fn name(&self) -> String {
        format!("\"{}\"", self.raw_name())
    }
    fn raw_name(&self) -> String;
    fn attributes(&self) -> String;
}

impl<Idx: IndexType + Display> DotState for DotStateInfo<Idx, bool> {
    fn raw_name(&self) -> String {
        format!("q{}", self.idx)
    }

    fn attributes(&self) -> String {
        format!(
            "label=\"{}\", shape=\"{}\"",
            self.raw_name(),
            if self.color { "doublecircle" } else { "circle" }
        )
    }
}

impl<Idx: IndexType + Display> DotState for DotStateInfo<Idx, usize> {
    fn raw_name(&self) -> String {
        format!("q{}", self.idx)
    }

    fn attributes(&self) -> String {
        format!("shape=circle, label=\"{}\"", self.raw_name())
    }
}

impl<Idx: IndexType + Display, S: Symbol + Display> DotState
    for DotStateInfo<Idx, crate::congruence::ColoredClass<S, bool>>
{
    fn raw_name(&self) -> String {
        format!("{}[{}]", self.prefix, self.idx)
    }

    fn attributes(&self) -> String {
        format!("shape=box, label=\"{}:{}\"", self.raw_name(), self.color)
    }
}

impl<Idx: IndexType + Display, S: Symbol + Display> DotState
    for DotStateInfo<Idx, crate::congruence::ColoredClass<S, usize>>
{
    fn raw_name(&self) -> String {
        format!("{}[{}]", self.prefix, self.idx)
    }

    fn attributes(&self) -> String {
        format!("shape=box, label=\"{}:{}\"", self.raw_name(), self.color)
    }
}

impl<Idx: IndexType + Display, S: Symbol + Display> DotState
    for DotStateInfo<Idx, crate::congruence::ColoredClass<S, ()>>
{
    fn raw_name(&self) -> String {
        format!("{}[{}]", self.prefix, self.idx)
    }

    fn attributes(&self) -> String {
        format!("shape=box, label=\"{}\"", self.raw_name())
    }
}

pub struct DotTransitionInfo<Idx: IndexType, Q: Color, C: Color, A: Alphabet> {
    source: DotStateInfo<Idx, Q>,
    target: DotStateInfo<Idx, Q>,
    color: C,
    expression: A::Expression,
}

pub trait DotTransition {
    fn dot_statement(&self) -> String;
}

impl<Idx, Q, A> DotTransition for DotTransitionInfo<Idx, Q, usize, A>
where
    Idx: IndexType,
    Q: Color,
    A: Alphabet,
    A::Expression: Display,
    DotStateInfo<Idx, Q>: DotState,
{
    fn dot_statement(&self) -> String {
        format!(
            "{} -> {} [label={}:{}]",
            self.source.name(),
            self.target.name(),
            self.expression,
            self.color
        )
    }
}

impl<Idx, Q, A> DotTransition for DotTransitionInfo<Idx, Q, (), A>
where
    Idx: IndexType,
    Q: Color,
    A: Alphabet,
    A::Expression: Display,
    DotStateInfo<Idx, Q>: DotState,
{
    fn dot_statement(&self) -> String {
        format!(
            "{} -> {} [label={}]",
            self.source.name(),
            self.target.name(),
            self.expression,
        )
    }
}

/// Trait that encapsulates the functionality of converting an object
/// into a [graphviz](https://graphviz.org/) representation.
pub trait ToDot {
    /// Compute the graphviz representation, for more information on the DOT format,
    /// see the [graphviz documentation](https://graphviz.org/doc/info/lang.html).
    fn dot_representation(&self) -> String;

    /// Returns the header of the graphviz representation. This is the part that goes
    /// right after the `digraph A {` part.
    fn header(&self) -> String;
    /// Returns the body of the graphviz representation. This is the part that goes
    /// right after the header and before the closing `}`.
    fn body(&self, prefix: &str) -> String;

    /// Renders the object visually (as PNG) and returns a vec of bytes/u8s encoding
    /// the rendered image. This method is only available on the `graphviz` crate feature
    /// and makes use of temporary files.
    #[cfg(feature = "graphviz")]
    fn render(&self) -> Result<Vec<u8>, std::io::Error> {
        use std::io::{Read, Write};
        let dot = self.dot_representation();

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

    /// Similar to the [`Self::render()`] method, but returns a [`tempfile::TempPath`] instead
    /// of a buffer of bytes.
    #[cfg(feature = "graphviz")]
    fn render_tempfile(&self) -> Result<tempfile::TempPath, std::io::Error> {
        use tracing::trace;

        trace!("Outputting dot and rendering to png");
        let dot = self.dot_representation();
        println!("{dot}");
        render_dot_to_tempfile(dot.as_str())
    }

    /// First creates a rendered PNG using [`Self::render_tempfile()`], after which the rendered
    /// image is displayed using a locally installed image viewer (`eog` on linux, `qlmanage`
    /// i.e. quicklook on macos and nothing yet on windows).
    #[cfg(feature = "graphviz")]
    fn display_rendered(&self) -> Result<(), std::io::Error> {
        let rendered_path = self.render_tempfile()?;
        display_png(rendered_path)
    }
}

impl<Ts> ToDot for Ts
where
    Ts: FiniteState,
    DotStateInfo<Ts::StateIndex, Ts::StateColor>: DotState,
    DotTransitionInfo<Ts::StateIndex, Ts::StateColor, Ts::EdgeColor, Ts::Alphabet>: DotTransition,
{
    fn dot_representation(&self) -> String {
        format!("digraph A {{\n{}\n{}\n}}\n", self.header(), self.body(""))
    }

    fn header(&self) -> String {
        [
            "fontname=\"Helvetica,Arial,sans-serif\"\nrankdir=LR".to_string(),
            "node [shape=circle]".into(),
        ]
        .join("\n")
    }

    fn body(&self, prefix: &str) -> String {
        let mut state_map = self
            .state_indices()
            .map(|q| {
                (
                    q,
                    DotStateInfo {
                        color: self.state_color(q).unwrap(),
                        idx: q,
                        prefix: prefix.to_string(),
                    },
                )
            })
            .collect::<Map<_, _>>();

        let mut lines = state_map.values().map(|q| q.dot_statement()).collect_vec();

        for state in self.state_indices() {
            let q = state_map.get(&state).unwrap();
            if let Some(it) = self.edges_from(state) {
                for e in it {
                    let p = state_map.get(&e.target()).unwrap();
                    let info = DotTransitionInfo {
                        source: q.clone(),
                        target: p.clone(),
                        color: e.color(),
                        expression: e.expression().clone(),
                    };
                    lines.push(info.dot_statement());
                }
            }
        }
        lines.join("\n")
    }
}

// impl<A: Alphabet, Q, C, Idx> ToDot for BTS<A, Q, C, Idx>
// where
//     A::Symbol: Display,
//     Q: Debug + Color,
//     C: Debug + Color,
//     Idx: IndexType + Debug,
// {
//     fn dot_representation(&self) -> String {
//         format!("digraph A {{\n{}\n{}\n}}\n", self.header(), self.body(""))
//     }

//     fn header(&self) -> String {
//         [
//             "fontname=\"Helvetica,Arial,sans-serif\"\nrankdir=LR".to_string(),
//             "node [shape=none]".into(),
//         ]
//         .join("\n")
//     }

//     fn body(&self, prefix: &str) -> String {
//         let mut lines = vec![];
//         let to_consider = self.state_indices();

//         for state in to_consider {
//             if let Some(it) = self.edges_from(state) {
//                 for e in it {
//                     lines.push(format!(
//                         "\"{prefix}{state},{:?}\" -> \"{prefix}{},{:?}\" [label = \"{:?}\"]",
//                         self.state_color(state)
//                             .expect("Actually every state should be colored!"),
//                         e.target(),
//                         self.state_color(e.target())
//                             .expect("Actually every state should be colored!"),
//                         e.expression(),
//                         prefix = prefix
//                     ));
//                 }
//             }
//         }
//         lines.join("\n")
//     }
// }

// impl<A: Alphabet, Q, C> ToDot for RightCongruence<A, Q, C>
// where
//     A::Symbol: Display,
//     Q: Debug + Color,
//     C: Debug + Color,
// {
//     fn dot_representation(&self) -> String {
//         format!("digraph A {{\n{}\n{}\n}}\n", self.header(), self.body(""))
//     }

//     fn header(&self) -> String {
//         [
//             "fontname=\"Helvetica,Arial,sans-serif\"\nrankdir=LR".to_string(),
//             "node [shape=none]".into(),
//         ]
//         .join("\n")
//     }

//     fn body(&self, prefix: &str) -> String {
//         let mut lines = vec![format!("\"{prefix},init\" [label=\"\", shape=none]")];

//         lines.push(format!(
//             "\"{prefix},init\" -> \"{prefix},{}\" [style=\"solid\"]",
//             self.state_color(self.initial())
//                 .expect("The initial state must be colored!"),
//         ));

//         let to_consider = self.state_indices();

//         for state in to_consider {
//             if let Some(it) = self.edges_from(state) {
//                 for e in it {
//                     lines.push(format!(
//                         "\"{prefix},{}\" -> \"{prefix},{}\" [label = \"{:?}\"]",
//                         self.state_color(state)
//                             .expect("Actually every state should be colored!"),
//                         self.state_color(e.target())
//                             .expect("Actually every state should be colored!"),
//                         e.expression(),
//                         prefix = prefix
//                     ));
//                 }
//             }
//         }
//         lines.join("\n")
//     }
// }

impl<A: Alphabet, Q: Color + Debug, C: Color + Debug> ToDot for Vec<RightCongruence<A, Q, C>>
where
    A::Symbol: Display,
    DotStateInfo<usize, ColoredClass<A::Symbol, Q>>: DotState,
    DotTransitionInfo<usize, ColoredClass<A::Symbol, Q>, C, A>: DotTransition,
{
    fn dot_representation(&self) -> String {
        format!("digraph A {{\n{}\n{}\n}}\n", self.header(), self.body(""),)
    }

    fn header(&self) -> String {
        [
            "compound=true".to_string(),
            "fontname=\"Helvetica,Arial,sans-serif\"\nrankdir=LR".to_string(),
            "init [label=\"\", shape=none]".into(),
            "node [shape=rect]".into(),
        ]
        .join("\n")
    }

    fn body(&self, _prefix: &str) -> String {
        self.iter()
            .enumerate()
            .map(|(i, cong)| {
                format!(
                    "subgraph cluster_{} {{\n{}\n{}\n}}\n",
                    i,
                    cong.header(),
                    cong.body(&format!("{i}"))
                )
            })
            .join("\n")
    }
}

impl<A: Alphabet, Q: Color + Debug, C: Color + Debug> ToDot for FORC<A, Q, C>
where
    A::Symbol: Display,
    DotStateInfo<usize, ColoredClass<A::Symbol, Q>>: DotState,
    DotTransitionInfo<usize, ColoredClass<A::Symbol, Q>, C, A>: DotTransition,
{
    fn dot_representation(&self) -> String {
        format!("digraph A {{\n{}\n{}\n}}\n", self.header(), self.body(""),)
    }

    fn header(&self) -> String {
        [
            "compund=true".to_string(),
            "fontname=\"Helvetica,Arial,sans-serif\"\nrankdir=LR".to_string(),
            "init [label=\"\", shape=none]".into(),
            "node [shape=rect]".into(),
        ]
        .join("\n")
    }

    fn body(&self, _prefix: &str) -> String {
        let mut lines = self
            .progress
            .iter()
            .map(|(class, prc)| {
                format!(
                    "subgraph cluster_{} {{\n{}\n{}\n}}\n",
                    self.leading()
                        .state_color(*class)
                        .unwrap()
                        .class()
                        .mr_to_string(),
                    prc.header(),
                    prc.body(&class.to_string())
                )
            })
            .collect_vec();

        lines.push("init [label=\"\", shape=none]".to_string());
        let eps_prc = self
            .prc(&Class::epsilon())
            .expect("Must have at least the epsilon prc");
        lines.push(format!(
            "init -> \"{},init\" [style=\"solid\"]",
            eps_prc
                .state_color(eps_prc.initial())
                .expect("State should have a color")
        ));

        for state in self.leading.state_indices() {
            for &sym in self.leading.alphabet().universe() {
                if let Some(edge) = self.leading.transition(state, sym) {
                    let _source_prc = self
                        .prc(
                            self.leading
                                .state_color(state)
                                .expect("State should be colored")
                                .class(),
                        )
                        .expect("Must have a prc for every state");
                    let _target_prc = self
                        .prc(
                            self.leading
                                .state_color(edge.target())
                                .expect("State should be colored")
                                .class(),
                        )
                        .expect("Must have a prc for every state");
                    lines.push(format!(
                        "\"{},init\" -> \"{},init\" [label = \"{}\", style=\"dashed\", ltail=\"cluster_{}\", lhead=\"cluster_{}\"]",
                        self.leading.state_color(state).expect("State should be colored"),
                        self.leading.state_color(edge.target()).expect("State should be colored"),
                        sym,
                        self.leading.state_color(state).expect("State should be colored").class().mr_to_string(),
                        self.leading.state_color(edge.target()).expect("State should be colored").class().mr_to_string()
                    ));
                }
            }
        }

        lines.join("\n")
    }
}

/// Renders the given dot string to a png file and displays it using the default
/// image viewer on the system.
#[cfg(feature = "graphviz")]
pub fn display_dot(dot: &str) -> Result<(), std::io::Error> {
    display_png(render_dot_to_tempfile(dot)?)
}

#[cfg(feature = "graphviz")]
fn render_dot_to_tempfile(dot: &str) -> Result<tempfile::TempPath, std::io::Error> {
    use std::io::{Read, Write};

    let mut tempfile = tempfile::NamedTempFile::new()?;
    tempfile.write_all(dot.as_bytes())?;
    let tempfile_name = tempfile.path();

    let image_tempfile = tempfile::Builder::new().prefix(".png").tempfile()?;
    let image_tempfile_name = image_tempfile.into_temp_path();

    let mut child = std::process::Command::new("dot")
        .arg("-Tpng")
        .arg("-o")
        .arg(&image_tempfile_name)
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
        Ok(image_tempfile_name)
    }
}

#[cfg(feature = "graphviz")]
fn display_png(rendered_path: tempfile::TempPath) -> Result<(), std::io::Error> {
    #[cfg(target_os = "linux")]
    std::process::Command::new("eog")
        .arg(&rendered_path)
        .spawn()?
        .wait()?;
    #[cfg(target_os = "macos")]
    std::process::Command::new("qlmanage")
        .arg("-p")
        .arg(&rendered_path)
        .spawn()?
        .wait()?;
    #[cfg(target_os = "windows")]
    std::process::Command::new("cmd")
        .arg("/c")
        .arg(format!("start {}", rendered_path.display()))
        .spawn()?
        .wait()?;
    #[cfg(target_os = "windows")]
    std::thread::sleep(std::time::Duration::from_secs(1));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{alphabet, congruence::FORC, ts::Sproutable, Class, Pointed, RightCongruence};

    use super::ToDot;

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
        forc.render_to_file_name("/home/leon/test.png");
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
        three_congs.display_rendered();
    }
}
