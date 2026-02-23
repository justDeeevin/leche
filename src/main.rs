#[allow(non_camel_case_types)]
#[allow(dead_code)]
mod class_file;
mod descriptors;

use class_file::ClassFile;
use color_eyre::eyre::OptionExt;
use leche_parse::Parsed;
use std::fs::File;

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    Ok(())
}
