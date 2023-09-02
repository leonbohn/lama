use std::fmt::Display;

use itertools::Itertools;

use crate::{
    congurence::FORC, ts::FiniteState, Alphabet, Class, Pointed, RightCongruence, TransitionSystem,
};

use super::successor::IsTransition;

/// Trait that encapsulates the functionality of converting an object
/// into a [graphviz](https://graphviz.org/) representation.
pub trait ToDot {
    /// Compute the graphviz representation, for more information on the DOT format,
    /// see the [graphviz documentation](https://graphviz.org/doc/info/lang.html).
    fn dot_representation(&self) -> String;

    fn header(&self) -> String;
    fn body(&self, prefix: &str) -> String;

    /// Renders the object visually (as PNG) and returns a vec of bytes/u8s encoding
    /// the rendered image. This method is only available on the `graphviz` crate feature
    /// and makes use of temporary files.
    #[cfg(feature = "graphviz")]
    fn render(&self) -> Result<Vec<u8>, std::io::Error> {
        use std::{
            io::{Read, Write},
            path::Path,
        };
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

    #[cfg(feature = "graphviz")]
    fn render_to_file_name(&self, filename: &str) -> Result<(), std::io::Error> {
        use std::{
            io::{Read, Write},
            path::Path,
        };
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

    /// Similar to the [`render`] method, but returns a [`tempfile::TempPath`] instead
    /// of a buffer of bytes.
    #[cfg(feature = "graphviz")]
    fn render_tempfile(&self) -> Result<tempfile::TempPath, std::io::Error> {
        use std::{
            io::{Read, Write},
            path::Path,
        };
        use tracing::trace;

        trace!("Outputting dot and rendering to png");
        let dot = self.dot_representation();
        render_dot_to_tempfile(dot.as_str())
    }

    /// First creates a rendered PNG using [`render_tempfile`], after which the rendered
    /// image is displayed using a locally installed image viewer (`eog` on linux, `qlmanage`
    /// i.e. quicklook on macos and nothing yet on windows).
    #[cfg(feature = "graphviz")]
    fn display_rendered(&self) -> Result<(), std::io::Error> {
        use std::io::Write;

        let rendered_path = self.render_tempfile()?;
        display_png(rendered_path)
    }
}

impl<A: Alphabet> ToDot for RightCongruence<A>
where
    A::Symbol: Display,
{
    fn dot_representation(&self) -> String {
        format!("digraph A {{\n{}\n{}\n}}\n", self.header(), self.body(""),)
    }

    fn header(&self) -> String {
        [
            "fontname=\"Helvetica,Arial,sans-serif\"\nrankdir=LR".to_string(),
            "node [shape=none]".into(),
        ]
        .join("\n")
    }

    fn body(&self, prefix: &str) -> String {
        let mut lines = vec![format!("\"{prefix},init\" [label=\"\", shape=none]")];

        lines.push(format!(
            "\"{prefix},init\" -> \"{prefix},{}\" [style=\"solid\"]",
            self.state_color(self.initial()),
        ));

        let to_consider = self.state_indices();

        for state in to_consider {
            for &sym in self.alphabet().universe() {
                if let Some(edge) = self.transition(state, sym) {
                    lines.push(format!(
                        "\"{prefix},{}\" -> \"{prefix},{}\" [label = \"{}\"]",
                        self.state_color(state),
                        self.state_color(edge.target()),
                        sym
                    ));
                }
            }
        }
        lines.join("\n")
    }
}

impl<A: Alphabet> ToDot for Vec<RightCongruence<A>>
where
    A::Symbol: Display,
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

impl<A: Alphabet> ToDot for FORC<A>
where
    A::Symbol: Display,
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
                    class.mr_to_string(),
                    prc.header(),
                    prc.body(&class.to_string())
                )
            })
            .collect_vec();

        lines.push("init [label=\"\", shape=none]".to_string());
        let eps_prc = self
            .progress
            .get(&Class::epsilon())
            .expect("Must have at least the epsilon prc");
        lines.push(format!(
            "init -> \"{},init\" [style=\"solid\"]",
            eps_prc.state_color(eps_prc.initial())
        ));

        for state in self.leading.state_indices() {
            for &sym in self.leading.alphabet().universe() {
                if let Some(edge) = self.leading.transition(state, sym) {
                    let source_prc = self
                        .progress
                        .get(&self.leading.state_color(state))
                        .expect("Must have a prc for every state");
                    let target_prc = self
                        .progress
                        .get(&self.leading.state_color(edge.target()))
                        .expect("Must have a prc for every state");
                    lines.push(format!(
                        "\"{},init\" -> \"{},init\" [label = \"{}\", style=\"dashed\", ltail=\"cluster_{}\", lhead=\"cluster_{}\"]",
                        self.leading.state_color(state),
                        self.leading.state_color(edge.target()),
                        sym,
                        self.leading.state_color(state).mr_to_string(),
                        self.leading.state_color(edge.target()).mr_to_string()
                    ));
                }
            }
        }

        lines.join("\n")
    }
}

#[cfg(feature = "graphviz")]
pub fn display_dot(dot: &str) -> Result<(), std::io::Error> {
    display_png(render_dot_to_tempfile(dot)?)
}

#[cfg(feature = "graphviz")]
fn render_dot_to_tempfile(dot: &str) -> Result<tempfile::TempPath, std::io::Error> {
    use std::{
        io::{Read, Write},
        path::Path,
    };
    use tracing::trace;
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
    use crate::{
        alphabet::Simple, congurence::FORC, simple, ts::Sproutable, Alphabet, Class, Pointed,
        RightCongruence,
    };

    use super::ToDot;

    #[test]
    #[ignore]
    fn display_forc() {
        let alphabet = simple!('a', 'b');
        let mut cong = RightCongruence::new(alphabet.clone());
        let q0 = cong.initial();
        let q1 = cong.add_state(vec!['a'].into());
        cong.add_edge(q0, 'a', q1, ());
        cong.add_edge(q0, 'b', q0, ());
        cong.add_edge(q1, 'a', q0, ());
        cong.add_edge(q1, 'b', q1, ());

        let mut prc_e = RightCongruence::new(alphabet.clone());
        let e0 = prc_e.initial();
        let e1 = prc_e.add_state(vec!['a'].into());
        let e2 = prc_e.add_state(vec!['b'].into());
        prc_e.add_edge(e0, 'a', e1, ());
        prc_e.add_edge(e0, 'b', e2, ());
        prc_e.add_edge(e1, 'a', e1, ());
        prc_e.add_edge(e1, 'b', e2, ());
        prc_e.add_edge(e2, 'a', e2, ());
        prc_e.add_edge(e2, 'b', e2, ());

        let mut prc_a = RightCongruence::new(alphabet);
        let a0 = prc_a.initial();
        let a1 = prc_a.add_state(vec!['a'].into());
        let a2 = prc_a.add_state(vec!['b'].into());
        let a3 = prc_a.add_state(vec!['a', 'a'].into());
        prc_a.add_edge(a0, 'a', a1, ());
        prc_a.add_edge(a0, 'b', a2, ());
        prc_a.add_edge(a1, 'a', a3, ());
        prc_a.add_edge(a1, 'b', a2, ());
        prc_a.add_edge(a2, 'a', a1, ());
        prc_a.add_edge(a2, 'b', a2, ());
        prc_a.add_edge(a3, 'a', a3, ());
        prc_a.add_edge(a3, 'b', a3, ());

        let forc = FORC::from_iter(
            cong,
            [(Class::epsilon(), prc_e), (Class::singleton('a'), prc_a)]
                .iter()
                .cloned(),
        );
        forc.render_to_file_name("/home/leon/test.png");
    }

    #[test]
    #[ignore]
    fn dot_render_and_display() {
        let alphabet = simple!('a', 'b');
        let mut cong = RightCongruence::new(alphabet);
        let q0 = cong.initial();
        let q1 = cong.add_state(vec!['a'].into());
        cong.add_edge(q0, 'a', q1, ());
        cong.add_edge(q0, 'b', q0, ());
        cong.add_edge(q1, 'a', q0, ());
        cong.add_edge(q1, 'b', q1, ());

        cong.display_rendered();
        let three_congs = vec![cong.clone(), cong.clone(), cong];
        three_congs.display_rendered();
    }
}
