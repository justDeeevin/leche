mod attributes;

use crate::{
    class_file::attributes::BootstrapMethod,
    descriptors::{parse_field_descriptor, parse_method_descriptor},
};
use attributes::{ClassAttribute, FieldAttribute, MethodAttribute};
use bitflags::bitflags;
use leche_parse::Parsed;
use regex::Regex;
use std::{
    cell::Cell,
    io::{Error, ErrorKind, Read},
    rc::Rc,
};

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
    cached_bootstrap_methods_index: Cell<Option<usize>>,
}

impl Parsed for ClassFile {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
        if u4::parse(reader)? != MAGIC_NUMBER {
            return Err(Error::new(ErrorKind::InvalidData, "invalid magic number"));
        }

        let minor_version = u2::parse(reader)?;

        let major_version = u2::parse(reader)?;

        if !(45..=69).contains(&major_version) {
            return Err(Error::new(
                ErrorKind::InvalidData,
                format!("unsupported major version: {major_version}"),
            ));
        }

        if major_version >= 56 && minor_version != 0 && minor_version != u2::MAX {
            return Err(Error::new(
                ErrorKind::InvalidData,
                format!("invalid minor version: {minor_version}"),
            ));
        }

        let constant_pool = (0..(u2::parse(reader)? - 1))
            .flat_map(|_| cp_info::parse(reader))
            .collect::<Result<Rc<_>, _>>()?;

        let access_flags = ClassAccessFlags::parse(reader)?;

        let this_class = usize::parse(reader)? - 1;

        let super_class = Option::parse(reader)?.map(minus_one);

        let interfaces = (0..u2::parse(reader)?)
            .map(|_| usize::parse(reader).map(minus_one))
            .collect::<Result<_, _>>()?;

        let fields = (0..u2::parse(reader)?)
            .map(|_| field_info::parse(reader, &constant_pool))
            .collect::<Result<_, _>>()?;

        let methods = (0..u2::parse(reader)?)
            .map(|_| method_info::parse(reader, &constant_pool))
            .collect::<Result<_, _>>()?;

        let attributes: Rc<[ClassAttribute]> = (0..u2::parse(reader)?)
            .map(|_| ClassAttribute::parse(reader, &constant_pool))
            .collect::<Result<_, _>>()?;

        if reader.read(&mut [0; 2])? != 0 {
            return Err(Error::new(ErrorKind::InvalidData, "trailing data"));
        }

        let out = Self {
            minor_version,
            major_version,
            constant_pool,
            access_flags,
            this_class,
            super_class,
            interfaces,
            fields,
            methods,
            attributes,
            cached_bootstrap_methods_index: Cell::new(None),
        };

        for constant in out.constant_pool.as_ref() {
            out.verify_constant(constant)?;
        }

        Ok(out)
    }
}

impl ClassFile {
    fn uses_preview_features(&self) -> bool {
        self.minor_version == u2::MAX
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

    fn verify_constant(&self, constant: &cp_info) -> std::io::Result<()> {
        use cp_info::*;

        match constant {
            Utf8 { .. } | Integer { .. } | Float { .. } | Long { .. } | Double { .. } => {}
            Class { name_index } => {
                if !matches!(self.constant_pool[*name_index], Utf8 { .. }) {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "class name does not point to a Utf8 constant",
                    ));
                }
            }
            String { string_index } => {
                if !matches!(self.constant_pool[*string_index], Utf8 { .. }) {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "string does not point to a Utf8 constant",
                    ));
                }
            }
            Fieldref(RefInfo {
                class_index,
                name_and_type_index,
            }) => {
                if !matches!(self.constant_pool[*class_index], Class { .. }) {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "fieldref class index does not point to a Class constant",
                    ));
                }
                let NameAndType {
                    name_index,
                    descriptor_index,
                } = &self.constant_pool[*name_and_type_index]
                else {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "fieldref name_and_type_index does not point to a NameAndType constant",
                    ));
                };
                self.verify_name_and_type(Some(true), *name_index, *descriptor_index)?;
            }
            Methodref(RefInfo {
                class_index,
                name_and_type_index,
            })
            | InterfaceMethodref(RefInfo {
                class_index,
                name_and_type_index,
            }) => {
                if !matches!(self.constant_pool[*class_index], Class { .. }) {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "methodref class index does not point to a Class constant",
                    ));
                }
                let NameAndType {
                    name_index,
                    descriptor_index,
                } = &self.constant_pool[*name_and_type_index]
                else {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "methodref name_and_type_index does not point to a NameAndType constant",
                    ));
                };
                self.verify_name_and_type(Some(false), *name_index, *descriptor_index)?;
            }
            NameAndType {
                name_index,
                descriptor_index,
            } => {
                self.verify_name_and_type(None, *name_index, *descriptor_index)?;
            }
            MethodHandle {
                reference_kind,
                reference_index,
            } => {
                use ReferenceKind::*;

                let reference = &self.constant_pool[*reference_index];

                if match reference_kind {
                    GetField | GetStatic | PutField | PutStatic => {
                        !matches!(reference, Fieldref { .. })
                    }
                    InvokeVirtual | NewInvokeSpecial => !matches!(reference, Methodref { .. }),
                    InvokeStatic | InvokeSpecial => {
                        !matches!(reference, Methodref { .. } | InterfaceMethodref { .. })
                    }
                    InvokeInterface => !matches!(reference, InterfaceMethodref { .. }),
                } {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "MethodHandle reference_kind mismatch",
                    ));
                }
            }
            MethodType { descriptor_index } => {
                let Utf8(descriptor) = &self.constant_pool[*descriptor_index] else {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "method_type descriptor_index does not point to a Utf8 constant",
                    ));
                };
                if parse_method_descriptor(descriptor).is_err() {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "method_type descriptor is not a valid method descriptor",
                    ));
                }
            }
            Dynamic(DynamicInfo {
                bootstrap_method_attr_index,
                name_and_type_index,
            })
            | InvokeDynamic(DynamicInfo {
                bootstrap_method_attr_index,
                name_and_type_index,
            }) => {
                if self
                    .bootstrap_methods()
                    .get(*bootstrap_method_attr_index)
                    .is_none()
                {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "dynamic bootstrap_method_attr_index is out of bounds",
                    ));
                }
                let NameAndType {
                    name_index,
                    descriptor_index,
                } = &self.constant_pool[*name_and_type_index]
                else {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "dynamic name_and_type_index does not point to a NameAndType constant",
                    ));
                };
                self.verify_name_and_type(None, *name_index, *descriptor_index)?;
            }
            Module { name_index } => {
                let Utf8(name) = &self.constant_pool[*name_index] else {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "module name_index does not point to a Utf8 constant",
                    ));
                };
                if !is_valid_module_name(name) {
                    return Err(Error::new(ErrorKind::InvalidData, "invalid module name"));
                }
            }
            Package { name_index } => {
                let Utf8(name) = &self.constant_pool[*name_index] else {
                    return Err(Error::new(
                        ErrorKind::InvalidData,
                        "package name_index does not point to a Utf8 constant",
                    ));
                };
                if !is_valid_module_name(name) {
                    return Err(Error::new(ErrorKind::InvalidData, "invalid package name"));
                }
            }
            _ => todo!(),
        }

        Ok(())
    }

    fn verify_name_and_type(
        &self,
        is_field: Option<bool>,
        name_index: usize,
        descriptor_index: usize,
    ) -> std::io::Result<()> {
        let cp_info::Utf8(name) = &self.constant_pool[name_index] else {
            return Err(Error::new(
                ErrorKind::InvalidData,
                "name_and_type name_index does not point to a Utf8 constant",
            ));
        };
        let cp_info::Utf8(descriptor) = &self.constant_pool[descriptor_index] else {
            return Err(Error::new(
                ErrorKind::InvalidData,
                "name_and_type descriptor_index does not point to a Utf8 constant",
            ));
        };

        let Some(is_field) = is_field else {
            return Ok(());
        };

        if is_field {
            if parse_field_descriptor(descriptor).is_err() {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    "name_and_type descriptor is not a valid field descriptor",
                ));
            }
        } else {
            if parse_method_descriptor(descriptor).is_err() {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    "name_and_type descriptor is not a valid method descriptor",
                ));
            }
            if name.starts_with('<') && name.as_ref() != "<init>" {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    "name_and_type method name starts with '<' and is not '<init>'",
                ));
            }
        }

        Ok(())
    }

    fn bootstrap_methods(&self) -> Rc<[BootstrapMethod]> {
        if let Some(index) = self.cached_bootstrap_methods_index.get() {
            let ClassAttribute::BootstrapMethods(methods) = &self.attributes[index] else {
                unreachable!()
            };
            methods.clone()
        } else if let Some((index, methods)) =
            self.attributes
                .iter()
                .enumerate()
                .find_map(|(i, attr)| match attr {
                    ClassAttribute::BootstrapMethods(methods) => Some((i, methods.clone())),
                    _ => None,
                })
        {
            self.cached_bootstrap_methods_index.set(Some(index));
            methods
        } else {
            Rc::new([])
        }
    }
}

fn is_valid_module_name(name: &str) -> bool {
    name.chars()
        .all(|c| !('\u{0000}'..='\u{001f}').contains(&c))
        && Regex::new(r"^((\\@)|(\\\\)|(\\:)|[^\\@:])*$")
            .unwrap()
            .is_match(name)
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
            3 => Ok(OneOrTwo::One(Self::Integer(u32::parse(reader)?))),
            4 => Ok(OneOrTwo::One(Self::Float(f32::parse(reader)?))),
            // TODO: test 8-byte parsing
            5 => Ok(OneOrTwo::Two(
                Self::Long(u64::parse(reader)?),
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

#[derive(Debug, Parsed)]
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

#[derive(Debug, Parsed)]
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
