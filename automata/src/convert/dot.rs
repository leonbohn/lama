use itertools::Itertools;

use crate::{
    ts::{InputOf, IntoTransitions, StateOf, Transition, Trigger},
    Combined, Successor,
};

use super::{AnnotatesState, AnnotatesTransition, DisplayState, DisplaySymbol};

pub trait ToDot {
    fn to_dot(&self) -> String;

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

    #[cfg(feature = "graphviz")]
    fn render_tempfile(&self) -> Result<tempfile::TempPath, std::io::Error> {
        use std::{io::Write, path::Path};
        use tracing::trace;

        trace!("Outputting dot and rendering to png");
        let dot = self.to_dot();

        let mut tempfile = tempfile::NamedTempFile::new()?;
        tempfile.write_all(dot.as_bytes())?;
        let tempfile_name = tempfile.path();

        let image_tempfile = tempfile::Builder::new().suffix(".png").tempfile()?;
        let image_tempfile_name = image_tempfile.into_temp_path();

        tracing::trace!(
            "Rendering dot ({}) to png ({})",
            tempfile_name.display(),
            image_tempfile_name.display()
        );
        let mut child = std::process::Command::new("dot")
            .arg("-Tpng")
            .arg("-o")
            .arg(&image_tempfile_name)
            .arg(tempfile_name)
            .spawn()?;

        child.wait()?;
        Ok(image_tempfile_name)
    }

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
    let mut child = std::process::Command::new("eog")
        .arg(&rendered_path)
        .spawn()?;
    #[cfg(target_os = "macos")]
    let mut child = std::process::Command::new("qlmanage")
        .arg("-p")
        .arg(&rendered_path)
        .spawn()?;
    child.wait()?;
    Ok(())
}

impl<TS, ACC> ToDot for Combined<TS, ACC>
where
    TS: Successor + DisplayState + DisplaySymbol,
    ACC: AnnotatesTransition<TS::Q, TS::Sigma> + AnnotatesState<TS::Q>,
    for<'a> &'a TS: IntoTransitions<Q = StateOf<TS>, Sigma = InputOf<TS>>,
{
    fn to_dot(&self) -> String {
        let mut lines = vec![
            "fontname=\"Helvetica,Arial,sans-serif\"\nrankdir=LR".to_string(),
            "init [label=\"\", shape=none]".into(),
            "node [shape=circle]".into(),
            format!("init -> {} [style=\"solid\"]", self.initial),
        ];

        for transition in self.ts().into_transitions() {
            let source = transition.source().clone();
            let sym = transition.sym().clone();
            let target = transition.target().clone();
            lines.push(format!(
                "{} -> {} [label = \"{}\"]",
                self.acceptance().annotate_state(&source),
                self.acceptance().annotate_state(&target),
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
