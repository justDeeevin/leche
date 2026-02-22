#![allow(dead_code)]

#[allow(non_camel_case_types)]
mod class_file;
mod descriptors;

use class_file::ClassFile;
use color_eyre::eyre::OptionExt;
use leche_parse::Parsed;
use std::fs::File;

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let class = ClassFile::parse(&mut File::open(
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

    dbg!(&class);

    Ok(())
}
