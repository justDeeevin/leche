#[allow(non_camel_case_types)]
#[allow(dead_code)]
mod class_file;

use class_file::{ClassFile, Parsed};
use color_eyre::eyre::OptionExt;
use std::fs::File;

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let class = ClassFile::parse(File::open(
        std::env::args()
            .nth(1)
            .ok_or_eyre("No class file provided")?,
    )?)
    .map_err(|e| {
        if e.kind() == std::io::ErrorKind::UnexpectedEof {
            std::io::Error::new(e.kind(), "unexpected end of file")
        } else {
            e
        }
    })?;

    dbg!(class);

    Ok(())
}
