use std::fmt::Display;

use itertools::Itertools;

use crate::{
    output::Mapping,
    ts::{InputOf, IntoTransitions, StateOf, Transition, Trigger, TriggerOf},
    Combined, MealyMachine, Successor, Symbol, Transformer, Value,
};

use super::{AnnotatesState, AnnotatesTransition, DisplayState, DisplaySymbol};

/// Trait that encapsulates the functionality of converting an object
/// into a [graphviz](https://graphviz.org/) representation.
pub trait ToDot {
    /// Compute the graphviz representation, for more information on the DOT format,
    /// see the [graphviz documentation](https://graphviz.org/doc/info/lang.html).
    fn to_dot(&self) -> String;

    /// Renders the object visually (as PNG) and returns a vec of bytes/u8s encoding
    /// the rendered image. This method is only available on the `graphviz` crate feature
    /// and makes use of temporary files.
    #[cfg(feature = "graphviz")]
    fn render(&self) -> Result<Vec<u8>, std::io::Error> {
        use std::{
            io::{Read, Write},
            path::Path,
        };
        let dot = self.to_dot();

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
    fn render_tempfile(&self) -> Result<tempfile::TempPath, std::io::Error> {
        use std::{
            io::{Read, Write},
            path::Path,
        };
        use tracing::trace;

        trace!("Outputting dot and rendering to png");
        let dot = self.to_dot();

        let mut tempfile = tempfile::NamedTempFile::new()?;
        tempfile.write_all(dot.as_bytes())?;
        let tempfile_name = tempfile.path();

        let image_tempfile = tempfile::Builder::new().suffix(".png").tempfile()?;
        let image_tempfile_name = image_tempfile.into_temp_path();

        tracing::trace!(
            "Rendering {}\n{}\n to {}",
            tempfile_name.display(),
            dot,
            image_tempfile_name.display()
        );
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
    fn display_rendered(&self) -> Result<(), std::io::Error> {
        use std::io::Write;

        let rendered_path = self.render_tempfile()?;
        tracing::trace!("Opening rendered file {}", rendered_path.display());
        display_png(rendered_path)
    }
}

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

impl<TS, V> ToDot for Combined<TS, Mapping<TS::Q, V>>
where
    TS: Successor,
    V: Value + Display,
    for<'a> &'a TS: IntoTransitions<Q = StateOf<TS>, Sigma = InputOf<TS>>,
{
    fn to_dot(&self) -> String {
        let mut lines = vec![
            "fontname=\"Helvetica,Arial,sans-serif\"\nrankdir=LR".to_string(),
            "init [label=\"\", shape=none]".into(),
            "node [shape=circle]".into(),
            format!(
                "init -> \"{}|{}\" [style=\"solid\"]",
                self.initial,
                self.acceptance().apply(&self.initial)
            ),
        ];

        for transition in self.ts().into_transitions() {
            let source = transition.source().clone();
            let sym = transition.sym().clone();
            let target = transition.target().clone();
            lines.push(format!(
                "\"{}|{}\" -> \"{}|{}\" [label = \"{}\"]",
                &source,
                self.acceptance().apply(&source),
                &target,
                self.acceptance().apply(&target),
                sym
            ));
        }

        format!(
            "digraph A {{\n{}\n}}\n",
            lines.into_iter().map(|line| format!("{line};")).join("\n")
        )
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::{convert::dot::display_png, tests::one_mod_three_times_a_dfa};

    use super::ToDot;

    #[test]
    #[traced_test]
    fn dot_display() {
        let ts = one_mod_three_times_a_dfa();
        ts.display_rendered().unwrap();
    }
}
