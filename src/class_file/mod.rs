pub mod attributes;

use attributes::{ClassAttribute, FieldAttribute, MethodAttribute};
use bitflags::bitflags;
use leche_parse::Parsed;
use std::{
    io::{Error, ErrorKind, Read},
    rc::Rc,
};

#[cfg(doc)]
use crate::instruction::*;

type u1 = u8;
type u2 = u16;
type u4 = u32;

const MAGIC_NUMBER: u4 = 0xCAFEBABE;

const fn minus_one(i: usize) -> usize {
    i - 1
}

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
    pub methods: Rc<[method_info]>,
    pub attributes: Rc<[ClassAttribute]>,
}

impl Parsed for ClassFile {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
        use std::io::{Error, ErrorKind};

        if u4::parse(reader)? != MAGIC_NUMBER {
            return Err(Error::new(ErrorKind::InvalidData, "invalid magic number"));
        }

        let minor_version = u2::parse(reader)?;
        let major_version = u2::parse(reader)?;

        let constant_pool = (0..(u2::parse(reader)? - 1))
            .flat_map(|_| cp_info::parse(reader))
            .collect::<Result<Rc<_>, _>>()?;

        Ok(Self {
            minor_version,
            major_version,
            access_flags: ClassAccessFlags::parse(reader)?,
            this_class: usize::parse(reader)? - 1,
            super_class: Option::parse(reader)?.map(minus_one),
            interfaces: (0..u2::parse(reader)?)
                .map(|_| usize::parse(reader).map(minus_one))
                .collect::<Result<_, _>>()?,
            fields: (0..u2::parse(reader)?)
                .map(|_| field_info::parse(reader, &constant_pool))
                .collect::<Result<_, _>>()?,
            methods: (0..u2::parse(reader)?)
                .map(|_| method_info::parse(reader, &constant_pool))
                .collect::<Result<_, _>>()?,
            attributes: (0..u2::parse(reader)?)
                .map(|_| ClassAttribute::parse(reader, &constant_pool))
                .collect::<Result<_, _>>()?,
            constant_pool,
        })
    }
}

impl ClassFile {
    pub fn get_class(&self, index: usize) -> std::io::Result<&Rc<str>> {
        if let cp_info::Class { name_index } = &self.constant_pool[index] {
            if let cp_info::Utf8(name) = &self.constant_pool[*name_index] {
                Ok(name)
            } else {
                Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("class at index {index} is not a Utf8 constant"),
                ))
            }
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected class at index {index}"),
            ))
        }
    }

    pub fn get_utf8(&self, index: usize) -> std::io::Result<&Rc<str>> {
        if let cp_info::Utf8(name) = &self.constant_pool[index] {
            Ok(name)
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected Utf8 at index {index}"),
            ))
        }
    }

    pub fn get_int(&self, index: usize) -> std::io::Result<i32> {
        if let cp_info::Integer(value) = &self.constant_pool[index] {
            Ok(*value)
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected Integer at index {index}"),
            ))
        }
    }

    pub fn get_double(&self, index: usize) -> std::io::Result<f64> {
        if let cp_info::Double(value) = &self.constant_pool[index] {
            Ok(*value)
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected Double at index {index}"),
            ))
        }
    }

    pub fn get_float(&self, index: usize) -> std::io::Result<f32> {
        if let cp_info::Float(value) = &self.constant_pool[index] {
            Ok(*value)
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected Float at index {index}"),
            ))
        }
    }

    pub fn get_long(&self, index: usize) -> std::io::Result<i64> {
        if let cp_info::Long(value) = &self.constant_pool[index] {
            Ok(*value)
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected Long at index {index}"),
            ))
        }
    }

    pub fn get_name_and_type(&self, index: usize) -> std::io::Result<(&Rc<str>, usize)> {
        if let cp_info::NameAndType {
            name_index,
            descriptor_index,
        } = &self.constant_pool[index]
        {
            Ok((self.get_utf8(*name_index)?, *descriptor_index))
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected NameAndType at index {index}"),
            ))
        }
    }

    pub fn get_module(&self, index: usize) -> std::io::Result<&Rc<str>> {
        if let cp_info::Module { name_index } = &self.constant_pool[index] {
            if let cp_info::Utf8(name) = &self.constant_pool[*name_index] {
                Ok(name)
            } else {
                Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("module at index {index} is not a Utf8 constant"),
                ))
            }
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected module at index {index}"),
            ))
        }
    }

    pub fn get_package(&self, index: usize) -> std::io::Result<&Rc<str>> {
        if let cp_info::Package { name_index } = &self.constant_pool[index] {
            if let cp_info::Utf8(name) = &self.constant_pool[*name_index] {
                Ok(name)
            } else {
                Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("package at index {index} is not a Utf8 constant"),
                ))
            }
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected package at index {index}"),
            ))
        }
    }

    pub fn get_fieldref(&self, index: usize) -> std::io::Result<&RefInfo> {
        if let cp_info::Fieldref(info) = &self.constant_pool[index] {
            Ok(info)
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected Fieldref at index {index}"),
            ))
        }
    }

    pub fn get_methodref(&self, index: usize) -> std::io::Result<&RefInfo> {
        if let cp_info::Methodref(info) = &self.constant_pool[index] {
            Ok(info)
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected Methodref at index {index}"),
            ))
        }
    }

    pub fn get_interface_methodref(&self, index: usize) -> std::io::Result<&RefInfo> {
        if let cp_info::InterfaceMethodref(info) = &self.constant_pool[index] {
            Ok(info)
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected InterfaceMethodref at index {index}"),
            ))
        }
    }

    pub fn get_method_handle(&self, index: usize) -> std::io::Result<&RefInfo> {
        if let cp_info::MethodHandle {
            reference_kind,
            reference_index,
        } = &self.constant_pool[index]
        {
            use ReferenceKind::*;
            match reference_kind {
                GetField | GetStatic | PutField | PutStatic => self.get_fieldref(*reference_index),
                InvokeVirtual | NewInvokeSpecial => self.get_methodref(*reference_index),
                InvokeStatic | InvokeSpecial => {
                    let out = self.get_methodref(*reference_index);
                    if self.major_version >= 52 {
                        out.or_else(|_| self.get_interface_methodref(*reference_index))
                    } else {
                        out
                    }
                }
                InvokeInterface => self.get_interface_methodref(*reference_index),
            }
        } else {
            Err(Error::new(
                ErrorKind::InvalidData,
                format!("expected MethodHandle at index {index}"),
            ))
        }
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

        /// Treat superclass methods specially when invoked by the [`invokespecial`] instruction.
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
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    /// Used to denote the unusable second half of a two-word value (long or double)
    SecondHalf,
    Class {
        // TODO: Arrays are classes...
        //
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

    // TODO: what others
    //
    /// A dynamically-computed constant, an arbitrary value that is produced by the invocation of
    /// a bootstrap method in the course of an [`ldc`] instruction, among others.
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
    pub fn parse(reader: &mut impl Read) -> OneOrTwo<std::io::Result<Self>> {
        match Self::parse_inner(reader) {
            Ok(OneOrTwo::One(value)) => OneOrTwo::One(Ok(value)),
            Ok(OneOrTwo::Two(value1, value2)) => OneOrTwo::Two(Ok(value1), Ok(value2)),
            Err(err) => OneOrTwo::One(Err(err)),
        }
    }

    fn parse_inner(reader: &mut impl Read) -> std::io::Result<OneOrTwo<Self>> {
        use std::io::{Error, ErrorKind};

        match u1::parse(reader)? {
            1 => {
                let len = usize::parse(reader)?;
                Ok(OneOrTwo::One(Self::Utf8(
                    std::io::read_to_string((reader).take(len as u64))?.into(),
                )))
            }
            3 => Ok(OneOrTwo::One(Self::Integer(i32::parse(reader)?))),
            4 => Ok(OneOrTwo::One(Self::Float(f32::parse(reader)?))),
            // TODO: test 8-byte parsing
            5 => Ok(OneOrTwo::Two(
                Self::Long(i64::parse(reader)?),
                Self::SecondHalf,
            )),
            6 => Ok(OneOrTwo::Two(
                Self::Double(f64::parse(reader)?),
                Self::SecondHalf,
            )),
            7 => Ok(OneOrTwo::One(Self::Class {
                name_index: usize::parse(reader)? - 1,
            })),
            8 => Ok(OneOrTwo::One(Self::String {
                string_index: usize::parse(reader)? - 1,
            })),
            9 => Ok(OneOrTwo::One(Self::Fieldref(RefInfo::parse(reader)?))),
            10 => Ok(OneOrTwo::One(Self::Methodref(RefInfo::parse(reader)?))),
            11 => Ok(OneOrTwo::One(Self::InterfaceMethodref(RefInfo::parse(
                reader,
            )?))),
            12 => Ok(OneOrTwo::One(Self::NameAndType {
                name_index: usize::parse(reader)? - 1,
                descriptor_index: usize::parse(reader)? - 1,
            })),
            15 => Ok(OneOrTwo::One(Self::MethodHandle {
                reference_kind: ReferenceKind::parse(reader)?,
                reference_index: usize::parse(reader)? - 1,
            })),
            16 => Ok(OneOrTwo::One(Self::MethodType {
                descriptor_index: usize::parse(reader)? - 1,
            })),
            17 => Ok(OneOrTwo::One(Self::Dynamic(DynamicInfo::parse(reader)?))),
            18 => Ok(OneOrTwo::One(Self::InvokeDynamic(DynamicInfo::parse(
                reader,
            )?))),
            19 => Ok(OneOrTwo::One(Self::Module {
                name_index: usize::parse(reader)? - 1,
            })),
            20 => Ok(OneOrTwo::One(Self::Package {
                name_index: usize::parse(reader)? - 1,
            })),
            tag => Err(Error::new(
                ErrorKind::InvalidData,
                format!("invalid constant tag: {tag}"),
            )),
        }
    }
}

#[derive(Debug)]
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
#[derive(Debug, Clone, Copy, Parsed)]
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
    #[map(minus_one)]
    pub class_index: usize,
    /// Index into the constant pool at which a [`cp_info::NameAndType`] is found representing the
    /// name and descriptor of the reference.
    #[map(minus_one)]
    pub name_and_type_index: usize,
}

#[derive(Debug, Parsed)]
pub struct DynamicInfo {
    /// Index into the `bootstrap_methods` array of the bootstrap method table of this class file.
    ///
    /// This can be used for self-reference, which will cause a failure at the time of resolution.
    pub bootstrap_method_attr_index: usize,
    /// Index into the constant pool at which a [`cp_info::NameAndType`] is found representing the
    /// name and descriptor of the method.
    #[map(minus_one)]
    pub name_and_type_index: usize,
}

trait ParsedWithString: Sized {
    fn parse(reader: &mut impl Read, constant_pool: &[cp_info]) -> std::io::Result<Self>;
}

#[derive(Debug)]
pub struct member_info<AccessFlags, Attributes> {
    pub access_flags: AccessFlags,
    pub name_index: usize,
    pub descriptor_index: usize,
    pub attributes: Rc<[Attributes]>,
}

impl<F: Parsed, A: ParsedWithString> ParsedWithString for member_info<F, A> {
    fn parse(reader: &mut impl Read, constant_pool: &[cp_info]) -> std::io::Result<Self> {
        Ok(Self {
            access_flags: Parsed::parse(reader)?,
            name_index: usize::parse(reader)? - 1,
            descriptor_index: usize::parse(reader)? - 1,
            attributes: (0..u2::parse(reader)?)
                .map(|_| A::parse(reader, constant_pool))
                .collect::<Result<_, _>>()?,
        })
    }
}

pub type field_info = member_info<MemberAccessFlags, FieldAttribute>;
pub type method_info = member_info<MethodAccessFlags, MethodAttribute>;

#[derive(Debug, Clone, Copy, Parsed)]
pub struct MethodAccessFlags(u2);

bitflags! {
    impl MethodAccessFlags: u2 {
        const ACC_PUBLIC = 0x0001;
        const ACC_PRIVATE = 0x0002;
        const ACC_PROTECTED = 0x0004;
        const ACC_STATIC = 0x0008;
        const ACC_FINAL = 0x0010;
        const ACC_SYNCHRONIZED = 0x0020;
        const ACC_BRIDGE = 0x0040;
        const ACC_VARARGS = 0x0080;
        const ACC_NATIVE = 0x0100;
        const ACC_ABSTRACT = 0x0400;
        const ACC_STRICT = 0x0800;
        const ACC_SYNTHETIC = 0x1000;
    }
}

#[derive(Debug, Clone, Copy, Parsed)]
#[bitflags]
pub struct MemberAccessFlags(u2);

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
