use cfg_if::cfg_if;
use std::io::Read;

pub use leche_parse_macros::Parsed;

pub trait Parsed: Sized {
    fn parse(reader: impl Read) -> std::io::Result<Self>;
}

macro_rules! impl_parsed {
    ($($ty:ty: $size:literal),* $(,)?) => {
        $(
            impl Parsed for $ty {
                fn parse(mut reader: impl Read) -> std::io::Result<Self> {
                    let mut buf = [0; $size];
                    reader.read_exact(&mut buf)?;
                    Ok(Self::from_be_bytes(buf))
                }
            }
        )*
    }
}

impl_parsed!(i8: 1, i16: 2, i32: 4, i64: 8, i128: 16, u8: 1, u16: 2, u32: 4, u64: 8, u128: 16);

cfg_if! {
    if #[cfg(target_pointer_width = "16")] {
        impl_parsed!(usize: 2, isize: 2);
    } else if #[cfg(target_pointer_width = "32")] {
        impl_parsed!(usize: 4, isize: 4);
    } else if #[cfg(target_pointer_width = "64")] {
        impl_parsed!(usize: 8, isize: 8);
    }
}

impl Parsed for Option<usize> {
    fn parse(reader: impl Read) -> std::io::Result<Self> {
        match usize::parse(reader)? {
            0 => Ok(None),
            n => Ok(Some(n)),
        }
    }
}

impl<T: Parsed> Parsed for std::ops::Range<T> {
    fn parse(mut reader: impl Read) -> std::io::Result<Self> {
        Ok(T::parse(&mut reader)?..T::parse(reader)?)
    }
}
