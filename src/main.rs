#![allow(dead_code)]

#[allow(non_camel_case_types)]
mod class_file;
mod descriptors;
#[allow(non_upper_case_globals)]
mod instruction;

use class_file::ClassFile;
use leche_parse::Parsed;
use std::fs::File;

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let class_file = match std::env::args().nth(1) {
        Some(path) => ClassFile::parse(&mut File::open(path)?)?,
        None => ClassFile::parse(&mut std::io::stdin())?,
    };

    dbg!(&class_file);

    Ok(())
}
