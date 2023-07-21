use itertools::Itertools;

use crate::Alphabet;

use super::TransitionSystem;

/// Trait that encapsulates the functionality of converting an object
/// into a [graphviz](https://graphviz.org/) representation.
pub trait ToDot: TransitionSystem {
    /// Compute the graphviz representation, for more information on the DOT format,
    /// see the [graphviz documentation](https://graphviz.org/doc/info/lang.html).
    fn dot_representation(&self, with_initial: Option<Self::StateIndex>) -> String {
        let mut lines = vec![
            "fontname=\"Helvetica,Arial,sans-serif\"\nrankdir=LR".to_string(),
            "init [label=\"\", shape=none]".into(),
            "node [shape=rect]".into(),
        ];
        if let Some(initial) = with_initial {
            lines.push(format!(
                "init -> \"{}|{:?}\" [style=\"solid\"]",
                initial,
                self.state_color(initial)
            ));
        }

        let to_consider = if let Some(initial) = with_initial {
            self.reachable_state_indices_from(initial).collect()
        } else {
            self.state_indices()
        };

        for state in to_consider {
            for &sym in self.alphabet().universe() {
                if let Some(edge) = self.successor(state, sym) {
                    lines.push(format!(
                        "\"{}|{:?}\" -> \"{}|{:?}\" [label = \"{:?}\"]",
                        state,
                        self.state_color(state),
                        edge.target(),
                        self.state_color(edge.target()),
                        sym
                    ));
                }
            }
        }

        format!(
            "digraph A {{\n{}\n}}\n",
            lines.into_iter().map(|line| format!("{line};")).join("\n")
        )
    }

    /// Renders the object visually (as PNG) and returns a vec of bytes/u8s encoding
    /// the rendered image. This method is only available on the `graphviz` crate feature
    /// and makes use of temporary files.
    #[cfg(feature = "graphviz")]
    fn render(&self, with_initial: Option<Self::StateIndex>) -> Result<Vec<u8>, std::io::Error> {
        use std::{
            io::{Read, Write},
            path::Path,
        };
        let dot = self.dot_representation(with_initial);

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

    /// Similar to the [`render`] method, but returns a [`tempfile::TempPath`] instead
    /// of a buffer of bytes.
    #[cfg(feature = "graphviz")]
    fn render_tempfile(
        &self,
        with_initial: Option<Self::StateIndex>,
    ) -> Result<tempfile::TempPath, std::io::Error> {
        use std::{
            io::{Read, Write},
            path::Path,
        };
        use tracing::trace;

        trace!("Outputting dot and rendering to png");
        let dot = self.dot_representation(with_initial);

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

    /// First creates a rendered PNG using [`render_tempfile`], after which the rendered
    /// image is displayed using a locally installed image viewer (`eog` on linux, `qlmanage`
    /// i.e. quicklook on macos and nothing yet on windows).
    #[cfg(feature = "graphviz")]
    fn display_rendered(
        &self,
        with_initial: Option<Self::StateIndex>,
    ) -> Result<(), std::io::Error> {
        use std::io::Write;

        let rendered_path = self.render_tempfile(with_initial)?;
        display_png(rendered_path)
    }
}

impl<Ts: TransitionSystem> ToDot for Ts {}

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
    use crate::{alphabet::Simple, simple, ts::Sproutable, Alphabet, Pointed, RightCongruence};

    use super::ToDot;

    #[test]
    fn dot_render_and_display() {
        let alphabet = simple!('a', 'b');
        let mut cong = RightCongruence::new(alphabet);
        let q0 = cong.initial();
        let q1 = cong.add_state(vec!['a'].into());
        cong.add_edge(q0, 'a', q1, ());
        cong.add_edge(q0, 'b', q0, ());
        cong.add_edge(q1, 'a', q0, ());
        cong.add_edge(q1, 'b', q1, ());

        cong.display_rendered(Some(q0));
    }
}
