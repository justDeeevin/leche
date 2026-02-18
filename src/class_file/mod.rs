mod attributes;

use attributes::attribute_info;
use bitflags::bitflags;
use leche_parse::Parsed;
use std::{io::Read, rc::Rc};

type u1 = u8;
type u2 = u16;
type u4 = u32;

const MAGIC: u4 = 0xCAFEBABE;

#[derive(Debug)]
pub struct ClassFile {
    pub minor_version: u2,
    pub major_version: u2,
    pub constant_pool: Rc<[cp_info]>,
    pub access_flags: ClassAccessFlags,

    /// Index into the constant pool at which a [`cp_info::Class`] is found representing this class.
    pub this_class: usize,

    /// Index into the constant pool at which a [`cp_info::Class`] is found representing the direct
    /// superclass of this class.
    ///
    /// **This will only be unset if this class represents `Object`.**
    ///
    /// For interfaces, this must point to the `Object` class.
    pub super_class: Option<usize>,

    /// Indices into the constant pool at which [`cp_info::Class`]es are found representing the
    /// interfaces implemented by this class.
    pub interfaces: Rc<[usize]>,
    pub fields: Rc<[field_info]>,
}

impl Parsed for ClassFile {
    fn parse(mut reader: impl Read) -> std::io::Result<Self> {
        use std::io::{Error, ErrorKind};

        let mut u4 = [0; 4];
        let mut u2 = [0; 2];

        reader.read_exact(&mut u4)?;
        if u4::from_be_bytes(u4) != MAGIC {
            return Err(Error::new(ErrorKind::InvalidData, "invalid magic number"));
        }

        reader.read_exact(&mut u2)?;
        let minor_version = u2::from_be_bytes(u2);

        reader.read_exact(&mut u2)?;
        let major_version = u2::from_be_bytes(u2);

        if !(45..=69).contains(&major_version) {
            return Err(Error::new(
                ErrorKind::InvalidData,
                format!("unsupported major version: {major_version}"),
            ));
        }

        if major_version >= 56 && minor_version != 0 && minor_version != 65535 {
            return Err(Error::new(
                ErrorKind::InvalidData,
                format!("invalid minor version: {minor_version}"),
            ));
        }

        reader.read_exact(&mut u2)?;

        let constant_pool = (0..(u2::from_be_bytes(u2) - 1))
            .flat_map(|_| cp_info::parse(&mut reader))
            .collect::<Result<Rc<_>, _>>()?;

        let access_flags = ClassAccessFlags::parse(&mut reader)?;

        let this_class = usize::parse(&mut reader)? - 1;

        let super_class = usize::parse(&mut reader)?;

        let interfaces = (0..u2::parse(&mut reader)?)
            .map(|_| usize::parse(&mut reader).map(|i| i - 1))
            .collect::<Result<_, _>>()?;

        let fields = (0..u2::parse(&mut reader)?)
            .map(|_| Self::parse_field(&mut reader, &constant_pool))
            .collect::<Result<_, _>>()?;

        Ok(Self {
            minor_version,
            major_version,
            constant_pool,
            access_flags,
            this_class,
            super_class: if super_class == 0 {
                None
            } else {
                Some(super_class - 1)
            },
            interfaces,
            fields,
        })
    }
}

impl ClassFile {
    fn uses_preview_features(&self) -> bool {
        self.minor_version == 65535
    }

    fn is_constant_valid(&self, constant: cp_info) -> bool {
        use cp_info::*;

        match constant {
            Utf8 { .. }
            | Integer { .. }
            | Float { .. }
            | Long { .. }
            | Double { .. }
            | Class { .. }
            | String { .. }
            | Fieldref { .. }
            | Methodref { .. }
            | InterfaceMethodref { .. }
            | NameAndType { .. } => self.major_version > 45 || self.minor_version >= 3,
            MethodHandle { .. } | MethodType { .. } | InvokeDynamic { .. } => {
                self.major_version >= 51
            }
            Module { .. } | Package { .. } => self.major_version >= 53,
            Dynamic { .. } => self.major_version >= 55,
            SecondHalf => true,
        }
    }

    fn is_constant_loadable(&self, constant: cp_info) -> bool {
        use cp_info::*;

        match constant {
            Integer { .. }
            | Float { .. }
            | Long { .. }
            | Double { .. }
            | String { .. }
            | MethodHandle { .. }
            | MethodType { .. }
            | Dynamic { .. } => true,
            Class { .. } => self.major_version >= 49,
            _ => false,
        }
    }

    fn parse_field(
        mut reader: impl Read,
        constant_pool: &[cp_info],
    ) -> std::io::Result<field_info> {
        let mut u2 = [0; 2];

        let access_flags = MemberAccessFlags::parse(&mut reader)?;

        reader.read_exact(&mut u2)?;
        let name_index = u2::from_be_bytes(u2) as usize - 1;

        reader.read_exact(&mut u2)?;
        let descriptor_index = u2::from_be_bytes(u2) as usize - 1;

        let attributes = (0..u2::parse(&mut reader)?)
            .map(|_| attribute_info::parse(&mut reader, constant_pool))
            .collect::<Result<_, _>>()?;

        Ok(field_info {
            access_flags,
            name_index,
            descriptor_index,
            attributes,
        })
    }
}

#[derive(Debug, Clone, Copy, Parsed)]
#[bitflags]
pub struct ClassAccessFlags(u2);

bitflags! {
    impl ClassAccessFlags: u2 {
        /// Declared public; may be accessed from outside its package.
        const ACC_PUBLIC = 0x0001;

        /// Declared final; no subclasses allowed.
        ///
        /// Conflicts with `ACC_ABSTRACT`.
        const ACC_FINAL = 0x0010;

        // TODO: link this instruction
        /// Treat superclass methods specially when invoked by the `invokespecial` instruction.
        const ACC_SUPER = 0x0020;

        /// Is an interface, not a class.
        ///
        /// Requires `ACC_ABSTRACT`.
        ///
        /// Conflicts with:
        /// - `ACC_FINAL`
        /// - `ACC_SUPER`
        /// - `ACC_ENUM`
        /// - `ACC_MODULE`
        const ACC_INTERFACE = 0x0200;

        /// Declared abstract; must not be instantiated.
        ///
        /// Conflicts with `ACC_FINAL`.
        const ACC_ABSTRACT = 0x0400;

        /// Declared synthetic; not present in the source code.
        const ACC_SYNTHETIC = 0x1000;

        /// Declared as an annotation interface.
        ///
        /// Requires `ACC_INTERFACE`.
        const ACC_ANNOTATION = 0x2000;

        /// Declared as an enum class.
        const ACC_ENUM = 0x4000;

        /// Is a module, not a class or interface.
        const ACC_MODULE = 0x8000;
    }
}

#[derive(Debug)]
pub enum cp_info {
    Utf8(Rc<str>),
    Integer(u32),
    Float(f32),
    Long(u64),
    Double(f64),
    /// Used to denote the unusable second half of a two-word value (long or double)
    SecondHalf,
    Class {
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a
        /// valid binary class or interface name.
        name_index: usize,
    },
    String {
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found containing the
        /// string's value.
        string_index: usize,
    },

    /// `name_and_type_index` must point to a field descriptor.
    Fieldref(RefInfo),

    /// `name_and_type_index` must point to a method descriptor.
    /// If the descriptor starts with '<', then it **must** be the special name `<init>`, an
    /// instance initialization method returning void.
    Methodref(RefInfo),

    /// `name_and_type_index` must point to a method descriptor.
    InterfaceMethodref(RefInfo),
    NameAndType {
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing an
        /// unqualified field or method name or the special method name `<init>`.
        name_index: usize,
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a field
        /// or method descriptor.
        descriptor_index: usize,
    },
    MethodHandle {
        reference_kind: ReferenceKind,
        /// Index into the constant pool at which the reference is found. The kind of reference
        /// must be valid for `reference_kind`. See [`ReferenceKind`] for details.
        reference_index: usize,
    },
    MethodType {
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a
        /// method descriptor.
        descriptor_index: usize,
    },

    // TODO: link these instructions
    /// A dynamically-computed constant, an arbitrary value that is produced by the invocation of
    /// a bootstrap method in the course of an `ldc` instruction, among others.
    Dynamic(DynamicInfo),

    /// A dynamically-computed callsite, a callsite that is produced by the invocation of a
    /// bootstrap method in the course of an `invokedynamic` instruction.
    InvokeDynamic(DynamicInfo),

    /// Only permitted in a class file that declares a module; that is, `access_flags` has
    /// `ACC_MODULE` set.
    Module {
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a
        /// module name.
        name_index: usize,
    },

    /// Only permitted in a class file that declares a module; that is, `access_flags` has
    /// `ACC_MODULE` set.
    Package {
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a
        /// package name.
        name_index: usize,
    },
}

impl cp_info {
    pub fn parse(reader: impl Read) -> OneOrTwo<std::io::Result<Self>> {
        match Self::parse_inner(reader) {
            Ok(OneOrTwo::One(value)) => OneOrTwo::One(Ok(value)),
            Ok(OneOrTwo::Two(value1, value2)) => OneOrTwo::Two(Ok(value1), Ok(value2)),
            Err(err) => OneOrTwo::One(Err(err)),
        }
    }

    fn parse_inner(mut reader: impl Read) -> std::io::Result<OneOrTwo<Self>> {
        use std::io::{Error, ErrorKind};

        let mut tag = [0; 1];
        reader.read_exact(&mut tag)?;

        match tag[0] {
            1 => {
                let len = usize::parse(&mut reader)?;
                Ok(OneOrTwo::One(Self::Utf8(
                    std::io::read_to_string((&mut reader).take(len as u64))?.into(),
                )))
            }
            3 => {
                let mut int = [0; 4];
                reader.read_exact(&mut int)?;
                Ok(OneOrTwo::One(Self::Integer(u32::from_be_bytes(int))))
            }
            4 => {
                let mut float = [0; 4];
                reader.read_exact(&mut float)?;
                Ok(OneOrTwo::One(Self::Float(f32::from_be_bytes(float))))
            }
            // TODO: test 8-byte parsing
            5 => {
                let mut long = [0; 8];
                reader.read_exact(&mut long)?;
                Ok(OneOrTwo::Two(
                    Self::Long(u64::from_be_bytes(long)),
                    Self::SecondHalf,
                ))
            }
            6 => {
                let mut double = [0; 8];
                reader.read_exact(&mut double)?;
                Ok(OneOrTwo::Two(
                    Self::Double(f64::from_be_bytes(double)),
                    Self::SecondHalf,
                ))
            }
            7 => Ok(OneOrTwo::One(Self::Class {
                name_index: usize::parse(&mut reader)? - 1,
            })),
            8 => Ok(OneOrTwo::One(Self::String {
                string_index: usize::parse(&mut reader)? - 1,
            })),
            9 => Ok(OneOrTwo::One(Self::Fieldref(RefInfo::parse(&mut reader)?))),
            10 => Ok(OneOrTwo::One(Self::Methodref(RefInfo::parse(&mut reader)?))),
            11 => Ok(OneOrTwo::One(Self::InterfaceMethodref(RefInfo::parse(
                &mut reader,
            )?))),
            12 => {
                let mut u2 = [0; 2];

                reader.read_exact(&mut u2)?;
                let name_index = u2::from_be_bytes(u2) as usize - 1;

                reader.read_exact(&mut u2)?;
                let descriptor_index = u2::from_be_bytes(u2) as usize - 1;

                Ok(OneOrTwo::One(Self::NameAndType {
                    name_index,
                    descriptor_index,
                }))
            }
            15 => Ok(OneOrTwo::One(Self::MethodHandle {
                reference_kind: ReferenceKind::parse(&mut reader)?,
                reference_index: usize::parse(&mut reader)? - 1,
            })),
            16 => Ok(OneOrTwo::One(Self::MethodType {
                descriptor_index: usize::parse(&mut reader)? - 1,
            })),
            17 => Ok(OneOrTwo::One(Self::Dynamic(DynamicInfo::parse(
                &mut reader,
            )?))),
            18 => Ok(OneOrTwo::One(Self::InvokeDynamic(DynamicInfo::parse(
                &mut reader,
            )?))),
            19 => Ok(OneOrTwo::One(Self::Module {
                name_index: usize::parse(&mut reader)? - 1,
            })),
            20 => Ok(OneOrTwo::One(Self::Package {
                name_index: usize::parse(&mut reader)? - 1,
            })),
            tag => Err(Error::new(
                ErrorKind::InvalidData,
                format!("invalid constant tag: {tag}"),
            )),
        }
    }
}

pub enum OneOrTwo<T> {
    One(T),
    Two(T, T),
}

impl<T> IntoIterator for OneOrTwo<T> {
    type Item = T;
    type IntoIter = OneOrTwoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        let i = 0;

        match self {
            OneOrTwo::One(value) => OneOrTwoIter {
                i,
                value: OneOrTwo::One(Some(value)),
            },
            OneOrTwo::Two(value1, value2) => OneOrTwoIter {
                i,
                value: OneOrTwo::Two(Some(value1), Some(value2)),
            },
        }
    }
}

pub struct OneOrTwoIter<T> {
    i: u8,
    value: OneOrTwo<Option<T>>,
}

impl<T> Iterator for OneOrTwoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.value {
            OneOrTwo::One(value) => {
                if self.i == 0 {
                    self.i += 1;
                    value.take()
                } else {
                    None
                }
            }
            OneOrTwo::Two(value1, value2) => {
                if self.i == 0 {
                    self.i += 1;
                    value1.take()
                } else if self.i == 1 {
                    self.i += 1;
                    value2.take()
                } else {
                    None
                }
            }
        }
    }
}

/// Valid constants for each kind:
/// - `GetField`, `GetStatic`, `PutField`, `PutStatic`: [`cp_info::Fieldref`]
/// - `InvokeVirtual`, `NewInvokeSpecial`: [`cp_info::Methodref`]
/// - `InvokeStatic`, `InvokeSpecial`: [`cp_info::Methodref`]. **For ^v52.0**, either [`cp_info::Methodref`] or [`cp_info::InterfaceMethodref`]
/// - `InvokeInterface`: [`cp_info::InterfaceMethodref`]
///
/// For `InvokeVirtual`, `InvokeStatic`, `InvokeSpecial`, and `InvokeInterface`, the name of the
/// method must not be `<init>` or `<clinit>`.
///
/// For `NewInvokeSpecial`, the name of the method must be `<init>`.
#[derive(Debug, Parsed)]
#[repr(u8)]
pub enum ReferenceKind {
    GetField = 1,
    GetStatic,
    PutField,
    PutStatic,
    InvokeVirtual,
    InvokeStatic,
    InvokeSpecial,
    NewInvokeSpecial,
    InvokeInterface,
}

#[derive(Debug, Parsed)]
pub struct RefInfo {
    /// Index into the constant pool at which a [`cp_info::Class`] is found representing a class or
    /// interface type of which the reference is a member.
    #[map(|i| i - 1)]
    class_index: usize,
    /// Index into the constant pool at which a [`cp_info::NameAndType`] is found representing the
    /// name and descriptor of the reference.
    #[map(|i| i - 1)]
    name_and_type_index: usize,
}

#[derive(Debug, Parsed)]
pub struct DynamicInfo {
    /// Index into the `bootstrap_methods` array of the bootstrap method table of this class file.
    ///
    /// This can be used for self-reference, which will cause a failure at the time of resolution.
    #[map(|i| i - 1)]
    bootstrap_method_attr_index: usize,
    /// Index into the constant pool at which a [`cp_info::NameAndType`] is found representing the
    /// name and descriptor of the method.
    #[map(|i| i - 1)]
    name_and_type_index: usize,
}

#[derive(Debug)]
pub struct field_info {
    access_flags: MemberAccessFlags,

    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing an
    /// unqualified field name.
    name_index: usize,

    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a field
    /// descriptor.
    descriptor_index: usize,
    attributes: Rc<[attribute_info]>,
}

#[derive(Debug, Parsed)]
#[bitflags]
struct MemberAccessFlags(u2);

bitflags! {
    impl MemberAccessFlags: u2 {
        const ACC_PUBLIC = 0x0001;
        const ACC_PRIVATE = 0x0002;
        const ACC_PROTECTED = 0x0004;
        const ACC_STATIC = 0x0008;
        /// Conflicts with `ACC_VOLATILE`
        const ACC_FINAL = 0x0010;
        /// Conflicts with `ACC_FINAL`
        const ACC_VOLATILE = 0x0040;
        const ACC_TRANSIENT = 0x0080;
        const ACC_SYNTHETIC = 0x1000;
        const ACC_ENUM = 0x4000;
    }
}
