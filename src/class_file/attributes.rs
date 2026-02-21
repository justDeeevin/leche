use super::{Parsed, ParsedWithString, cp_info, member_info, minus_one, u1, u2, u4};
use bitflags::bitflags;
use leche_parse::collect_with_len_type;
use std::{
    io::{Error, ErrorKind, Read},
    ops::Range,
    rc::Rc,
};

fn get_constant_string(constant_pool: &[cp_info], index: usize) -> std::io::Result<&Rc<str>> {
    match constant_pool.get(index).ok_or_else(|| {
        Error::new(
            ErrorKind::InvalidData,
            format!("attribute name index {index} is out of bounds"),
        )
    })? {
        cp_info::Utf8(s) => Ok(s),
        constant => Err(Error::new(
            ErrorKind::InvalidData,
            format!("constant at index {index} for attribute is not Utf8; found: {constant:?}"),
        )),
    }
}

#[derive(Debug)]
pub enum MemberAttribute<TypeAnnotationTarget> {
    /// Indicates a class member that does not appear in the source code.
    ///
    /// Not valid on [`record_component_info`].
    Synthetic,

    // TODO: actually figure out how signatures work
    //
    /// There can be at most one `Signature` attribute in a member.
    Signature {
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a
        /// signature.
        signature_index: usize,
    },
    /// Not valid on [`record_component_info`].
    Deprecated,
    /// There can be at most one `RuntimeVisibleAnnotations` attribute in a member.
    RuntimeVisibleAnnotations(Rc<[annotation]>),
    /// There can be at most one `RuntimeInvisibleAnnotations` attribute in a member.
    RuntimeInvisibleAnnotations(Rc<[annotation]>),
    /// There can be at most one `RuntimeVisibleTypeAnnotations` attribute in a member.
    RuntimeVisibleTypeAnnotations(Rc<[type_annotation<TypeAnnotationTarget>]>),
    /// There can be at most one `RuntimeInvisibleTypeAnnotations` attribute in a member.
    RuntimeInvisibleTypeAnnotations(Rc<[type_annotation<TypeAnnotationTarget>]>),
    Unknown(Rc<str>),
}

impl<T: Parsed> ParsedWithString for MemberAttribute<T> {
    fn parse(reader: &mut impl Read, constant_pool: &[cp_info]) -> std::io::Result<Self> {
        let name = get_constant_string(constant_pool, usize::parse(reader)? - 1)?;

        let _attribute_length = u4::parse(reader)?;

        match name.as_ref() {
            "Synthetic" => Ok(Self::Synthetic),
            "Signature" => Ok(Self::Signature {
                signature_index: usize::parse(reader)? - 1,
            }),
            "Deprecated" => Ok(Self::Deprecated),
            "RuntimeVisibleAnnotations" => {
                Ok(Self::RuntimeVisibleAnnotations(Parsed::parse(reader)?))
            }
            "RuntimeInvisibleAnnotations" => {
                Ok(Self::RuntimeInvisibleAnnotations(Parsed::parse(reader)?))
            }
            "RuntimeVisibleTypeAnnotations" => {
                Ok(Self::RuntimeVisibleTypeAnnotations(Parsed::parse(reader)?))
            }
            "RuntimeInvisibleTypeAnnotations" => Ok(Self::RuntimeInvisibleTypeAnnotations(
                Parsed::parse(reader)?,
            )),
            _ => Ok(Self::Unknown(name.clone())),
        }
    }
}

#[derive(Debug, Parsed)]
pub struct type_annotation<T> {
    pub target: T,
    pub target_path: type_path,
}

#[derive(Debug)]
pub struct type_path(pub Rc<[TypePathElement]>);

impl Parsed for type_path {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
        Ok(Self(collect_with_len_type::<u1, _, _>(reader)?))
    }
}

#[derive(Debug, Parsed)]
pub struct TypePathElement {
    pub type_path_kind: TypePathKind,
    /// See
    /// <https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-4.html#jvms-4.7.20.2-220-B-A.1>
    pub type_argument_index: u1,
}

#[derive(Debug, Parsed)]
#[repr(u8)]
pub enum TypePathKind {
    Array,
    Nested,
    WildcardTypeArgumentBound,
    TypeArgument,
}

#[derive(Debug, Parsed)]
#[repr(u8)]
pub enum ClassTypeAnnotationTarget {
    /// Type parameter declaration of a generic class or interface.
    ClassTypeParameter(u1),
    /// Type in the `extends` or `implements` clause of a class or interface declaration.
    Supertype(supertype_target) = 0x10,
    /// Type in a bound of a type parameter declaration of a generic class or interface.
    ClassTypeParameterBound(type_parameter_bound_target),
}

#[derive(Debug, Parsed)]
#[repr(u8)]
pub enum MethodTypeAnnotationTarget {
    /// Type parameter declaration of a generic method or constructor.
    MethodTypeParameter(u1) = 0x01,
    /// Type in a bound of a type parameter declaration of a generic method or constructor.
    MethodTypeParameterBound(type_parameter_bound_target) = 0x12,
    /// Return type of a method, or type of a newly constructed object.
    Return = 0x14,
    /// Receiver type a of method or constructor.
    Receiver,
    /// Type in a formal parameter declaration of a method or constructor.
    Parameter(u1),
    /// Type in the `throws` clause of a method or constructor.
    Throws(usize),
}

#[derive(Debug, Parsed)]
#[repr(u8)]
/// For both [`field_info`](super::field_info) and [`record_component_info`].
pub enum FieldTypeAnnotationTarget {
    /// Type in a field or record component declaration.
    Field = 0x13,
}

#[derive(Debug, Parsed)]
#[repr(u8)]
pub enum CodeTypeAnnotationTarget {
    /// Type in a local variable declaration.
    Local(Rc<[LocalAnnotationTarget]>) = 0x40,
    /// Type in a resource variable declaration.
    Resource(Rc<[LocalAnnotationTarget]>),
    /// Type in an exception parameter declaration.
    ExceptionParameter(usize),
    /// Type in an `instanceof` expression.
    Instanceof(usize),
    /// Type in a `new` expression.
    New(usize),
    /// Type in a method reference expression using `::new`.
    NewMethod(usize),
    /// Type in a method reference expression using `::Identifier`.
    IdentifierMethod(usize),
    /// Type in a cast expression.
    Cast(type_argument_target),
    /// Type argument for a generic constructor in a `new` expression or an explicit constructor
    /// invocation statement.
    ConstructorTypeArgument(type_argument_target),
    /// Type argument for a generic method in a method invocation expression.
    MethodTypeArgument(type_argument_target),
    /// Type argument for a generic constructor in a method reference expression using `::new`.
    ConstructorMethodTypeArgument(type_argument_target),
    /// Type argument for a generic method in a method reference expression using `::Identifier`.
    IdentifierMethodTypeArgument(type_argument_target),
}

#[derive(Debug, Parsed)]
pub struct type_argument_target {
    pub offset: usize,
    pub type_argument_index: u1,
}

#[derive(Debug, Parsed)]
pub struct LocalAnnotationTarget {
    pub scope: Range<usize>,
    pub index: usize,
}

#[derive(Debug, Parsed)]
pub struct type_parameter_bound_target {
    pub type_parameter_index: u1,
    pub bound_index: u1,
}

#[derive(Debug)]
pub enum supertype_target {
    Superclass,
    Interface(usize),
}

impl Parsed for supertype_target {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
        let value = u2::parse(reader)?;
        Ok(if value == u2::MAX {
            Self::Superclass
        } else {
            Self::Interface(value as usize)
        })
    }
}

#[derive(Debug, Parsed)]
pub struct annotation {
    #[map(minus_one)]
    type_index: usize,
    element_value_pairs: Rc<[ElementValuePair]>,
}

#[derive(Debug, Parsed)]
pub struct ElementValuePair {
    #[map(minus_one)]
    element_name_index: usize,
    value: element_value,
}

#[derive(Debug, Parsed)]
#[repr(u8)]
pub enum element_value {
    /// Points to an integer.
    Byte(usize) = b'B',
    /// Points to an integer.
    Char(usize) = b'C',
    /// Points to a double.
    Double(usize) = b'D',
    /// Points to a float.
    Float(usize) = b'F',
    /// Points to an integer.
    Int(usize) = b'I',
    /// Points to a long.
    Long(usize) = b'J',
    /// Points to an integer.
    Short(usize) = b'S',
    /// Points to an integer.
    Bool(usize) = b'Z',
    /// Points to a Utf8.
    String(usize) = b's',
    Enum {
        #[map(minus_one)]
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a
        /// field descriptor.
        type_name_index: usize,
        #[map(minus_one)]
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a
        /// simple name.
        const_name_index: usize,
    } = b'e',
    /// Points to a Utf8 representing a return descriptor.
    Class(usize) = b'c',
    Annotation(annotation) = b'@',
    Array(Rc<[Self]>) = b'[',
}

#[derive(Debug)]
pub enum ClassAttribute {
    /// There can be at most one `SourceFile` attribute per class.
    SourceFile {
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a
        /// source file name.
        sourcefile_index: usize,
    },

    /// Lists inner classes.
    ///
    /// Each [`cp_info::Class`] in the constant pool must have one corresponding entry in the
    /// inner classes table.
    InnerClasses(Rc<[InnerClass]>),

    /// A class can only have an `EnclosingMethod` if it represents a local or anonymous class.
    ///
    /// There can be at most one `EnclosingMethod` attribute per class.
    EnclosingMethod {
        /// Index into the constant pool at which a [`cp_info::Class`] is found representing an
        /// enclosing class.
        class_index: usize,
        /// Index into the constant pool at which a [`cp_info::NameAndType`] is found representing
        /// the name and descriptor of a method.
        method_index: Option<usize>,
    },

    /// Extra debugging information that is not semantically relevant.
    ///
    /// There can be at most one `SourceDebugExtension` attribute per class.
    SourceDebugExtension(Rc<str>),

    /// Bootstrap methods used to produce dynamically-computed constants and dynamically-computed
    /// callsites.
    ///
    /// There must be exactly one `BootstrapMethods` attribute in a class if it has any
    /// [`cp_info::Dynamic`] or [`cp_info::InvokeDynamic`] entries in its constant pool.
    BootstrapMethods(Rc<[BootstrapMethod]>),
    /// There can be at most one `Module` attribute per class.
    Module(ModuleAttribute),

    /// Indices into the constant pool at which [`cp_info::Package`]es are found representing a
    /// package in the current module.
    ///
    /// There can be at most one `ModulePackages` attribute per class.
    ModulePackages(Rc<[usize]>),

    /// Index into the constant pool at which a [`cp_info::Class`] is found representing the
    /// main class of the current module.
    ///
    /// There can be at most one `ModuleMainClass` attribute per class.
    ModuleMainClass(usize),

    /// Index into the constant pool at which a [`cp_info::Class`] is found representing the
    /// nest host of the nest to which the current class or interface claims to belong.
    ///
    /// Conflicts with `NestMembers`.
    ///
    /// There can be at most one `NestHost` attribute per class.
    NestHost(usize),

    /// Indices into the constant pool at which [`cp_info::Class`]es are found representing the
    /// classes and interfaces that are authorized to claim membership in the nest hosted by the
    /// current class or interface.
    ///
    /// Conflicts with `NestHost`.
    ///
    /// There can be at most one `NestMembers` attribute per class.
    NestMembers(Rc<[usize]>),

    /// Indicates that this is a record class.
    ///
    /// There can be at most one `Record` attribute per class.
    Record(Rc<[record_component_info]>),

    /// For a sealed class, indices into the constant pool at which [`cp_info::Class`]es are found
    /// representing its permitted subclasses.
    ///
    /// There can be at most one `PermittedSubclasses` attribute per class.
    ///
    /// Invalid on classes with [`ACC_FINAL`](super::ClassAccessFlags::ACC_FINAL) set.
    PermittedSubclasses(Rc<[usize]>),
    Member(MemberAttribute<ClassTypeAnnotationTarget>),
}

impl ParsedWithString for ClassAttribute {
    fn parse(reader: &mut impl Read, constant_pool: &[cp_info]) -> std::io::Result<Self> {
        let name = get_constant_string(constant_pool, usize::parse(reader)? - 1)?;

        let attribute_length = u4::parse(reader)?;

        match name.as_ref() {
            "SourceFile" => Ok(Self::SourceFile {
                sourcefile_index: usize::parse(reader)? - 1,
            }),
            "InnerClasses" => Ok(Self::InnerClasses(Parsed::parse(reader)?)),
            "EnclosingMethod" => Ok(Self::EnclosingMethod {
                class_index: usize::parse(reader)? - 1,
                method_index: Option::<_>::parse(reader)?.map(minus_one),
            }),
            "SourceDebugExtension" => Ok(Self::SourceDebugExtension(
                std::io::read_to_string((reader).take(attribute_length as u64))?.into(),
            )),
            "BootstrapMethods" => Ok(Self::BootstrapMethods(Parsed::parse(reader)?)),
            "Module" => Ok(Self::Module(Parsed::parse(reader)?)),
            "ModulePackages" => Ok(Self::ModulePackages(
                (0..u2::parse(reader)?)
                    .map(|_| usize::parse(reader).map(minus_one))
                    .collect::<Result<_, _>>()?,
            )),
            "ModuleMainClass" => Ok(Self::ModuleMainClass(usize::parse(reader)? - 1)),
            "NestHost" => Ok(Self::NestHost(usize::parse(reader)? - 1)),
            "NestMembers" => Ok(Self::NestMembers(
                (0..u2::parse(reader)?)
                    .map(|_| usize::parse(reader).map(minus_one))
                    .collect::<Result<_, _>>()?,
            )),
            "Record" => Ok(Self::Record(
                (0..u2::parse(reader)?)
                    .map(|_| record_component_info::parse(reader, constant_pool))
                    .collect::<Result<_, _>>()?,
            )),
            "PermittedSubclasses" => Ok(Self::PermittedSubclasses(
                (0..u2::parse(reader)?)
                    .map(|_| usize::parse(reader).map(minus_one))
                    .collect::<Result<_, _>>()?,
            )),
            _ => Ok(Self::Member(MemberAttribute::parse(reader, constant_pool)?)),
        }
    }
}

pub type record_component_info = member_info<(), MemberAttribute<FieldTypeAnnotationTarget>>;

#[derive(Debug, Parsed)]
pub struct ModuleAttribute {
    #[map(minus_one)]
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing the name of
    /// the current module.
    module_name_index: usize,
    // TODO: bitflags
    module_flags: ParameterAccessFlags,
    #[map(|o| o.map(minus_one))]
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing the
    /// version of the current module.
    module_version_index: Option<usize>,
    /// Dependencies
    requires: Rc<[Require]>,
    /// Exported packages
    exports: Rc<[Export]>,
    /// Opened packages
    opens: Rc<[Export]>,
    /// Indices into the constant pool at which [`cp_info::Class`]es are found representing service
    /// interfaces which the current module may discover via `java.util.ServiceLoader`.`
    uses_index: UsesIndex,
    /// Service implementations
    provides: Rc<[Provide]>,
}

#[derive(Debug)]
pub struct Provide {
    /// Index into the constant pool at which a [`cp_info::Class`] is found representing the
    /// service interface.
    provides_index: usize,
    /// Indices into the constant pool at which [`cp_info::Class`]es are found representing the
    /// service implementations.
    provides_with_index: Rc<[usize]>,
}

impl Parsed for Provide {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
        let provides_index = usize::parse(reader)? - 1;
        let provides_with_index = (0..u2::parse(reader)?)
            .map(|_| usize::parse(reader).map(minus_one))
            .collect::<Result<_, _>>()?;

        Ok(Self {
            provides_index,
            provides_with_index,
        })
    }
}

#[derive(Debug)]
pub struct UsesIndex(Rc<[usize]>);

impl Parsed for UsesIndex {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
        Ok(Self(
            (0..u2::parse(reader)?)
                .map(|_| usize::parse(reader).map(minus_one))
                .collect::<Result<_, _>>()?,
        ))
    }
}

#[derive(Debug, Parsed)]
pub struct Require {
    #[map(minus_one)]
    /// Index into the constant pool at which a [`cp_info::Module`] is found representing the
    /// required module.
    requires_index: usize,
    requires_flags: RequireFlags,
    #[map(|o| o.map(minus_one))]
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing the
    /// version of the required module.
    requires_version_index: Option<usize>,
}

#[derive(Debug, Parsed)]
#[bitflags]
pub struct RequireFlags(u2);

bitflags! {
    impl RequireFlags: u2 {
        const ACC_TRANSITIVE = 0x0020;
        const ACC_STATIC_PHASE = 0x0040;
        const ACC_SYNTHETIC = 0x1000;
        const ACC_MANDATED = 0x8000;
    }
}

#[derive(Debug)]
pub struct Export {
    /// Index into the constant pool at which a [`cp_info::Package`] is found representing the name of
    /// the exported package.
    exports_index: usize,
    exports_flags: ExportFlags,
    /// Indices into the constant pool at which [`cp_info::Module`]es are found representing the
    /// modules that can access the types and members in this exported package.
    exports_to_index: Rc<[usize]>,
}

#[derive(Debug, Parsed)]
#[bitflags]
pub struct ExportFlags(u2);

bitflags! {
    impl ExportFlags: u2 {
        const ACC_SYNTHETIC = 0x1000;
        const ACC_MANDATED = 0x8000;
    }
}

impl Parsed for Export {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
        let exports_index = usize::parse(reader)? - 1;
        let exports_flags = Parsed::parse(reader)?;
        let exports_to_index = (0..u2::parse(reader)?)
            .map(|_| usize::parse(reader).map(minus_one))
            .collect::<Result<_, _>>()?;

        Ok(Self {
            exports_index,
            exports_flags,
            exports_to_index,
        })
    }
}

#[derive(Debug, Parsed)]
pub struct BootstrapMethod {
    /// Index into the constant pool at which a [`cp_info::MethodHandle`] is found representing the
    /// bootstrap method.
    #[map(minus_one)]
    bootstrap_method_ref: usize,
    /// Indices into the constant pool at which loadable constants are found.
    bootstrap_arguments: Rc<[usize]>,
}

#[derive(Debug, Parsed)]
pub struct InnerClass {
    /// Index into the constant pool at which a [`cp_info::Class`] is found representing the
    /// inner class.
    #[map(minus_one)]
    inner_class_info_index: usize,
    /// Index into the constant pool at which a [`cp_info::Class`] is found representing the outer
    /// class, if any.
    ///
    /// Must be `None` if anonymous for ^v51.0.
    #[map(|o| o.map(minus_one))]
    outer_class_info_index: Option<usize>,
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing the simple
    /// name of the inner class.
    ///
    /// `None` if anonymous.
    #[map(|o| o.map(minus_one))]
    inner_name_index: Option<usize>,
    inner_class_access_flags: InnerClassAccessFlags,
}

#[derive(Debug, Parsed)]
#[bitflags]
struct InnerClassAccessFlags(u2);

bitflags! {
    impl InnerClassAccessFlags: u2 {
        const ACC_PUBLIC = 0x0001;
        const ACC_PRIVATE = 0x0002;
        const ACC_PROTECTED = 0x0004;
        const ACC_STATIC = 0x0008;
        const ACC_FINAL = 0x0010;
        const ACC_INTERFACE = 0x0200;
        const ACC_ABSTRACT = 0x0400;
        const ACC_SYNTHETIC = 0x1000;
        const ACC_ANNOTATION = 0x2000;
        const ACC_ENUM = 0x4000;
    }
}

#[derive(Debug)]
pub enum FieldAttribute {
    /// If `ACC_STATIC` is set, then the field represented by the parent [`field_info`](super::field_info) is assigned
    /// the value at `constantvalue_index` as part of the initialization of the class or interface
    /// declaring the field.
    ///
    /// There can be at most one `ConstantValue` attribute per field.
    ConstantValue {
        constantvalue_index: usize,
    },
    Member(MemberAttribute<FieldTypeAnnotationTarget>),
}

impl ParsedWithString for FieldAttribute {
    fn parse(reader: &mut impl Read, constant_pool: &[cp_info]) -> std::io::Result<Self> {
        let name = get_constant_string(constant_pool, usize::parse(reader)? - 1)?;

        let _attribute_length = u4::parse(reader)?;

        match name.as_ref() {
            "ConstantValue" => Ok(Self::ConstantValue {
                constantvalue_index: usize::parse(reader)? - 1,
            }),
            _ => Ok(Self::Member(MemberAttribute::parse(reader, constant_pool)?)),
        }
    }
}

#[derive(Debug)]
pub enum MethodAttribute {
    Code {
        /// Maximum depth of the operand stack.
        max_stack: u2,

        /// Number of local variables upon invocation, including parameters.
        ///
        ///  The greatest local variable index for a value of type long or double is max_locals - 2.
        ///  The greatest local variable index for a value of any other type is max_locals - 1.
        max_locals: u2,

        // TODO: probably should make an enum for bytecode

        // When the code array is read into memory on a byte-addressable machine, if the first byte
        // of the array is aligned on a 4-byte boundary, the tableswitch and lookupswitch 32-bit
        // offsets will be 4-byte aligned. (Refer to the descriptions of those instructions for
        // more information on the consequences of code array alignment.)
        code: Rc<[u1]>,
        exception_table: Rc<[ExceptionHandler]>,
        attributes: Rc<[CodeAttribute]>,
    },

    /// Indicates which checked exceptions a method may throw.
    ///
    /// There can be at most one `Exceptions` attribute per method.
    Exceptions {
        /// Indices into the constant pool at which [`cp_info::Class`]es are found representing the
        /// exceptions thrown by this method.
        exception_index_table: Rc<[usize]>,
    },
    RuntimeVisibleParameterAnnotations(Rc<[Rc<[annotation]>]>),
    RuntimeInvisibleParameterAnnotations(Rc<[Rc<[annotation]>]>),

    /// On a method representing an element of an annotation interface, records the default value.
    ///
    /// There can be at most one `AnnotationDefault` attribute per method.
    AnnotationDefault(element_value),
    MethodParameters(Rc<[MethodParameter]>),
    Member(MemberAttribute<MethodTypeAnnotationTarget>),
}

impl ParsedWithString for MethodAttribute {
    fn parse(reader: &mut impl Read, constant_pool: &[cp_info]) -> std::io::Result<Self> {
        let name = get_constant_string(constant_pool, usize::parse(reader)? - 1)?;

        let _attribute_length = u4::parse(reader)?;

        match name.as_ref() {
            "Code" => Ok(Self::Code {
                max_stack: u2::parse(reader)?,
                max_locals: u2::parse(reader)?,
                code: collect_with_len_type::<u4, _, _>(reader)?,
                exception_table: Parsed::parse(reader)?,
                attributes: (0..u2::parse(reader)?)
                    .map(|_| CodeAttribute::parse(reader, constant_pool))
                    .collect::<Result<_, _>>()?,
            }),
            "Exceptions" => Ok(Self::Exceptions {
                exception_index_table: (0..u2::parse(reader)?)
                    .map(|_| usize::parse(reader).map(minus_one))
                    .collect::<Result<_, _>>()?,
            }),
            "RuntimeVisibleParameterAnnotations" => Ok(Self::RuntimeVisibleParameterAnnotations(
                collect_with_len_type::<u1, _, _>(reader)?,
            )),
            "RuntimeInvisibleParameterAnnotations" => {
                Ok(Self::RuntimeInvisibleParameterAnnotations(
                    collect_with_len_type::<u1, _, _>(reader)?,
                ))
            }
            "AnnotationDefault" => Ok(Self::AnnotationDefault(Parsed::parse(reader)?)),
            "MethodParameters" => Ok(Self::MethodParameters(Parsed::parse(reader)?)),
            _ => Ok(Self::Member(MemberAttribute::parse(reader, constant_pool)?)),
        }
    }
}

#[derive(Debug, Parsed)]
pub struct MethodParameter {
    pub name_index: usize,
    pub access_flags: ParameterAccessFlags,
}

#[derive(Debug, Parsed)]
#[bitflags]
pub struct ParameterAccessFlags(u2);

bitflags! {
    impl ParameterAccessFlags: u2 {
        const ACC_FINAL = 0x0010;
        const ACC_SYNTHETIC = 0x1000;
        const ACC_MANDATED = 0x8000;
    }
}

#[derive(Debug, Parsed)]
pub struct ExceptionHandler {
    /// Range of indices in the code array in which the exception handler is active.
    ///
    /// Referred to as `start_pc` and `end_pc` in the JVM spec.
    active: Range<usize>,

    /// Index of the first instruction of the exception handler in the code array.
    handler_pc: u2,

    /// An index into the constant pool at which a [`cp_info::Class`] is found representing the
    /// exception type.
    ///
    /// If `None`, the exception handler catches all exceptions.
    #[map(|o| o.map(minus_one))]
    catch_type: Option<usize>,
}

#[derive(Debug)]
pub enum CodeAttribute {
    /// Used for type checking.
    ///
    /// For ^v50.0, if `Code` doesn't have a `StackMapTable`, an empty table is implied.
    ///
    /// There can be at most one `StackMapTable` per `Code`.
    StackMapTable(Rc<[stack_map_frame]>),
    /// Used to map instructions to lines of source code.
    LineNumberTable(Rc<[LineNumber]>),

    /// Used to map local variables to their names.
    ///
    /// There can be no more than one [`LocalVariable`] per local variable.
    LocalVariableTable(Rc<[LocalVariable]>),
    /// There can be no more than one [`LocalVariableType`] per local variable.
    LocalVariableTypeTable(Rc<[LocalVariableType]>),
    Unknown(Rc<str>),
}

impl ParsedWithString for CodeAttribute {
    fn parse(reader: &mut impl Read, constant_pool: &[cp_info]) -> std::io::Result<Self> {
        let name = get_constant_string(constant_pool, usize::parse(reader)? - 1)?;

        let _attribute_length = u4::parse(reader)?;

        match name.as_ref() {
            "StackMapTable" => Ok(Self::StackMapTable(Parsed::parse(reader)?)),
            "LineNumberTable" => Ok(Self::LineNumberTable(Parsed::parse(reader)?)),
            "LocalVariableTable" => Ok(Self::LocalVariableTable(Parsed::parse(reader)?)),
            "LocalVariableTypeTable" => Ok(Self::LocalVariableTypeTable(Parsed::parse(reader)?)),
            _ => Ok(Self::Unknown(name.clone())),
        }
    }
}

#[derive(Debug, Parsed)]
pub struct LocalVariable {
    /// Range of indices in the code array within which the local variable has a value.
    scope: Range<usize>,
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing the
    /// unqualified name of a local variable.
    #[map(minus_one)]
    name_index: usize,
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing the field
    /// descriptor of a local variable.
    #[map(minus_one)]
    descriptor_index: usize,
    /// Index into the current frame's local variable array at which a local variable is located.
    index: usize,
}

#[derive(Debug, Parsed)]
pub struct LocalVariableType {
    /// Range of indices in the code array within which the local variable has a value.
    scope: Range<usize>,
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing the
    /// unqualified name of a local variable.
    #[map(minus_one)]
    name_index: usize,
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing the field
    /// descriptor of a local variable's type.
    #[map(minus_one)]
    descriptor_index: usize,
    /// Index into the current frame's local variable array at which a local variable is located.
    index: usize,
}

#[derive(Debug, Parsed)]
pub struct LineNumber {
    /// Index of the first instruction of the line in the code array.
    start_pc: usize,
    // TODO: should this be usize?
    line_number: u2,
}

#[derive(Debug)]
pub struct stack_map_frame {
    offset_delta: u2,
    info: stack_map_frame_info,
}

impl Parsed for stack_map_frame {
    fn parse(reader: &mut impl Read) -> std::io::Result<Self> {
        let tag = u1::parse(reader)?;

        match tag {
            0..=63 => Ok(Self {
                offset_delta: tag as u2,
                info: stack_map_frame_info::same_frame,
            }),
            64..=127 => Ok(Self {
                offset_delta: tag as u2 - 64,
                info: stack_map_frame_info::same_locals_1_stack_item_frame {
                    stack: verification_type_info::parse(reader)?,
                },
            }),
            247 => Ok(Self {
                offset_delta: u2::parse(reader)?,
                info: stack_map_frame_info::same_locals_1_stack_item_frame {
                    stack: verification_type_info::parse(reader)?,
                },
            }),
            248..=250 => Ok(Self {
                offset_delta: u2::parse(reader)?,
                info: stack_map_frame_info::chop_frame { removed: 251 - tag },
            }),
            251 => Ok(Self {
                offset_delta: u2::parse(reader)?,
                info: stack_map_frame_info::same_frame,
            }),
            252..=254 => Ok(Self {
                offset_delta: u2::parse(reader)?,
                info: stack_map_frame_info::append_frame {
                    locals: (0..(tag - 251))
                        .map(|_| verification_type_info::parse(reader))
                        .collect::<Result<_, _>>()?,
                },
            }),
            255 => Ok(Self {
                offset_delta: u2::parse(reader)?,
                info: stack_map_frame_info::full_frame {
                    locals: Parsed::parse(reader)?,
                    stack: Parsed::parse(reader)?,
                },
            }),
            _ => Err(Error::new(
                ErrorKind::InvalidData,
                format!("invalid stack map frame tag: {tag}"),
            )),
        }
    }
}

#[derive(Debug)]
enum stack_map_frame_info {
    /// Same locals as previous frame; empty operand stack.
    same_frame,
    /// Same locals as previous frame; operand stack has one entry.
    same_locals_1_stack_item_frame {
        // Technically a single-item array in the spec but this saves me some time
        stack: verification_type_info,
    },
    /// Same locals as previous frame **with the last `removed` locals removed**; empty operand
    /// stack.
    chop_frame { removed: u1 },
    /// Same locals as previous frame with locals appended; empty operand stack.
    append_frame {
        locals: Rc<[verification_type_info]>,
    },
    full_frame {
        locals: Rc<[verification_type_info]>,
        stack: Rc<[verification_type_info]>,
    },
}

#[derive(Debug, Parsed)]
enum verification_type_info {
    Top,
    Integer,
    Float,
    Double,
    Long,
    Null,
    UninitializedThis,
    Object {
        /// Index into the constant pool at which a [`cp_info::Class`] is found representing the
        /// class of the object.
        #[map(minus_one)]
        cpool_index: usize,
    },
    Uninitialized {
        /// Offset in the code array of the `new` instruction that created the object.
        offset: u2,
    },
}
