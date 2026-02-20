use cfg_if::cfg_if;
use num_traits::Zero;
use std::io::Read;

pub use leche_parse_macros::Parsed;

pub trait Parsed: Sized {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self>;
}

macro_rules! impl_parsed {
    ($($ty:ty: $size:literal),* $(,)?) => {
        $(
            impl Parsed for $ty {
                fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
                    let mut buf = [0; ::std::mem::size_of::<Self>()];
                    reader.read_exact(&mut buf)?;
                    Ok(Self::from_be_bytes(buf))
                }
            }
        )*
    }
}

impl_parsed!(i8: 1, i16: 2, i32: 4, i64: 8, i128: 16, u8: 1, u16: 2, u32: 4, u64: 8, u128: 16);

impl Parsed for usize {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
        Ok(u16::parse(reader)? as usize)
    }
}

cfg_if! {
    if #[cfg(target_pointer_width = "16")] {
        impl_parsed!(isize: 2);
    } else if #[cfg(target_pointer_width = "32")] {
        impl_parsed!(isize: 4);
    } else if #[cfg(target_pointer_width = "64")] {
        impl_parsed!(isize: 8);
    }
}

impl Parsed for Option<usize> {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
        match usize::parse(reader)? {
            0 => Ok(None),
            n => Ok(Some(n)),
        }
    }
}

impl<T: Parsed> Parsed for std::ops::Range<T> {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
        Ok(T::parse(reader)?..T::parse(reader)?)
    }
}

impl<T: Parsed> Parsed for std::rc::Rc<[T]> {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
        (0..usize::parse(reader)?)
            .map(|_| T::parse(reader))
            .collect()
    }
}

pub fn collect_with_len_type<L: Parsed + Zero, T: Parsed, C: FromIterator<T>>(
    reader: &mut impl Read,
) -> std::io::Result<C>
where
    std::ops::Range<L>: Iterator<Item = L>,
{
    (L::zero()..L::parse(reader)?)
        .map(|_| T::parse(reader))
        .collect()
}
