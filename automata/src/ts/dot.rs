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

/// Stores all necessary information for introducing and referencing a node in the DOT format.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DotStateData {
    name: String,
    attributes: Vec<DotStateAttribute>,
}

impl DotStateData {
    fn dot_statement(&self) -> String {
        format!(
            "{} [{}]",
            self.dot_name(),
            self.attributes.iter().map(|o| o.to_string()).join(", ")
        )
    }

    /// Appends the given attribute.
    pub fn push_attribute(&mut self, attr: DotStateAttribute) {
        self.attributes.push(attr);
    }

    fn dot_name(&self) -> String {
        format!("\"{}\"", self.name)
    }

    /// Returns a pointer to the raw name of the node.
    pub fn raw_name(&self) -> &str {
        &self.name
    }
}

impl<Idx: Display> From<(&str, Idx)> for DotStateData {
    fn from((prefix, value): (&str, Idx)) -> Self {
        let name = match prefix.len() {
            0 => value.to_string(),
            _prefix_len => format!("{prefix}|{}", value),
        };
        Self {
            name,
            attributes: vec![],
        }
    }
}

/// Trait that encapsulates the functionality of coloring/annotating a state in a transition system.
pub trait DotStateColorize {
    /// Takes a mutable reference to an instance of [`DotStateData`] and modifies it to reflect
    /// the information provided by `self`. If `self` is, for example, a boolean, this has the effect
    /// of drawing the node with a double circle. If `self` is a usize, this has the effect of
    /// adding a label to the node.
    fn dot_state_colorize(&self, base: &mut DotStateData);
}

impl DotStateColorize for bool {
    fn dot_state_colorize(&self, base: &mut DotStateData) {
        base.attributes.push(DotStateAttribute::Shape(
            if *self { "doublecircle" } else { "circle" }.to_string(),
        ));
    }
}

impl DotStateColorize for usize {
    fn dot_state_colorize(&self, base: &mut DotStateData) {
        base.attributes
            .push(DotStateAttribute::Label(format!("{}:{}", base.name, self)))
    }
}

impl DotStateColorize for () {
    fn dot_state_colorize(&self, base: &mut DotStateData) {}
}

impl<S: Symbol + Display, C: Color + DotStateColorize> DotStateColorize
    for crate::congruence::ColoredClass<S, C>
{
    fn dot_state_colorize(&self, base: &mut DotStateData) {
        base.name = format!("[{}]", self.class().mr_to_string());
        base.attributes
            .push(DotStateAttribute::Shape("box".to_string()));
        self.color().dot_state_colorize(base);
    }
}

/// Stores all necessary information for producing a DOT statement, which draws a singular
/// transition between two nodes.
pub struct DotTransitionInfo<C: Color, A: Alphabet> {
    source_name: String,
    target_name: String,
    color: C,
    expression: A::Expression,
}

/// Implemented for all types that can be used to draw a transition between two nodes in the DOT format.
pub trait DotTransition {
    /// Returns the actual DOT statement of the transition.
    fn dot_statement(&self) -> String;
}

impl<A> DotTransition for DotTransitionInfo<usize, A>
where
    A: Alphabet,
    A::Expression: Display,
{
    fn dot_statement(&self) -> String {
        format!(
            "{} -> {} [label=\"{}:{}\"]",
            self.source_name, self.target_name, self.expression, self.color
        )
    }
}

impl<A> DotTransition for DotTransitionInfo<(), A>
where
    A: Alphabet,
    A::Expression: Display,
{
    fn dot_statement(&self) -> String {
        format!(
            "{} -> {} [label={}]",
            self.source_name, self.target_name, self.expression,
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
    Ts::StateColor: DotStateColorize,
    DotTransitionInfo<Ts::EdgeColor, Ts::Alphabet>: DotTransition,
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
                let mut info = (prefix, q).into();
                self.state_color(q).unwrap().dot_state_colorize(&mut info);
                (q, info)
            })
            .collect::<Map<_, DotStateData>>();

        let mut lines = state_map.values().map(|q| q.dot_statement()).collect_vec();

        for state in self.state_indices() {
            let q = state_map.get(&state).unwrap();
            if let Some(it) = self.edges_from(state) {
                for e in it {
                    let p = state_map.get(&e.target()).unwrap();
                    let info = DotTransitionInfo {
                        source_name: q.dot_name().to_string(),
                        target_name: p.dot_name().to_string(),
                        color: e.color(),
                        expression: e.expression().clone(),
                    };
                    lines.push(info.dot_statement());
                }
            }
        }

        // deal with the initial state if it exists
        if let Some(initial) = self.maybe_initial_state() {
            let q = state_map.get(&initial).unwrap();
            lines.push("init [shape=\"none\", label=\" \"]".to_string());
            lines.push(format!("init -> {}", q.dot_name()));
        }

        lines.join("\n")
    }
}

impl<A: Alphabet, Q: Color + Debug, C: Color + Debug> ToDot for Vec<RightCongruence<A, Q, C>>
where
    A::Symbol: Display,
    Q: DotStateColorize,
    DotTransitionInfo<C, A>: DotTransition,
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
    Q: DotStateColorize,
    DotTransitionInfo<C, A>: DotTransition,
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

    let image_tempfile = tempfile::Builder::new().suffix(".png").tempfile()?;
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
    use crate::{
        alphabet, congruence::FORC, prelude::DPA, ts::Sproutable, Class, Pointed, RightCongruence,
    };

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

    #[test]
    #[ignore]
    fn dot_render_dpa() {
        let alphabet = alphabet!(simple 'a', 'b');
        let mut dpa = DPA::new(alphabet);
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
