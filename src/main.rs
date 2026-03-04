#![allow(dead_code)]

#[allow(non_camel_case_types)]
mod class_file;
mod descriptors;
#[allow(non_upper_case_globals)]
mod instruction;

use class_file::ClassFile;
use std::fs::File;

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let class = if let Some(path) = std::env::args().nth(1) {
        ClassFile::new(File::open(path)?)?
    } else {
        ClassFile::new(std::io::stdin())?
    };

    dbg!(&class);

    Ok(())
}
