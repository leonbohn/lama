use std::io::Read;

pub fn to_file_or_stdout(maybe_file_name: Option<&String>, output: &str) -> anyhow::Result<()> {
    if let Some(file_name) = maybe_file_name {
        std::fs::write(file_name, output)?;
        Ok(())
    } else {
        println!("{output}");
        Ok(())
    }
}

pub fn from_file_or_stdin(maybe_file_name: Option<&String>) -> String {
    match maybe_file_name {
        Some(f) => std::fs::read_to_string(f).expect("Unable to read file"),
        None => {
            let mut buf = String::new();
            std::io::stdin().read_to_string(&mut buf);
            buf
        }
    }
}

pub fn read_from_file_or_stdin(maybe_file_name: Option<&String>) -> Box<dyn Read> {
    if let Some(file_name) = maybe_file_name {
        Box::new(std::fs::File::open(file_name).expect("Could not open input file"))
    } else {
        Box::new(std::io::stdin())
    }
}
