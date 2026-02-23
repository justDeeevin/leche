mod raw;

use crate::descriptors::{FieldDescriptor, MethodDescriptor};
use bitflags::bitflags;
use std::{ops::Range, rc::Rc};

pub use raw::{
    ClassAccessFlags as ClassFlags,
    attributes::{
        FieldTypeAnnotationTarget, LineNumber, MethodTypeAnnotationTarget, TypePathElement,
    },
};

pub struct ClassFile {
    pub major_version: u16,
    pub minor_version: u16,
    pub constants: Rc<[Constant]>,
    pub flags: ClassFlags,
    pub this: Rc<str>,
    pub superclass: Option<Rc<str>>,
    pub interfaces: Rc<[Rc<str>]>,
    pub fields: Rc<[Field]>,
    pub methods: Rc<[Method]>,
    pub source_file: Option<Rc<str>>,
    pub inner_classes: Rc<[InnerClass]>,
    pub enclosing_method: Option<EnclosingMethod>,
    pub source_debug_extension: Option<Rc<str>>,
    pub bootstrap_methods: Rc<[BootstrapMethod]>,
    pub module: Option<Module>,
    pub module_packages: Rc<[Rc<str>]>,
    pub module_main_class: Option<Rc<str>>,
    pub nest: Option<Nest>,
    pub record: Option<Rc<[Member<FieldDescriptor, FieldTypeAnnotationTarget>]>>,
    /// Invalid for final classes
    pub permitted_subclasses: Option<Rc<[Rc<str>]>>,
}

pub enum Nest {
    Host(Rc<str>),
    Members(Rc<[Rc<str>]>),
}

pub struct Module {
    pub name: Rc<str>,
    pub flags: ParameterFlags,
    pub module_version: Option<Rc<str>>,
    pub requires: Rc<[Require]>,
    pub exports: Rc<[Export]>,
    pub opens: Rc<[Export]>,
    pub uses: Rc<[Rc<str>]>,
    pub provides: Rc<[Provide]>,
}

pub struct Provide {
    pub class: Rc<str>,
    pub provides_with: Rc<[Rc<str>]>,
}

pub struct Export {
    pub name: Rc<str>,
    pub flags: ExportFlags,
    pub exports_to: Rc<[Rc<str>]>,
}

bitflags! {
    pub struct ExportFlags: u8 {
        const SYNTHETIC = 1 << 0;
        const MANDATED = 1 << 1;
    }
}

pub struct Require {
    pub name: Rc<str>,
    pub flags: RequireFlags,
    pub version: Option<Rc<str>>,
}

bitflags! {
    pub struct RequireFlags: u8 {
        const TRANSITIVE = 1 << 0;
        const STATIC_PHASE = 1 << 1;
        const SYNTHETIC = 1 << 2;
        const MANDATED = 1 << 3;
    }
}

pub struct BootstrapMethod {
    pub method: Ref<MethodDescriptor>,
    pub arguments: Rc<[Constant]>,
}

pub struct Ref<Descriptor> {
    pub class: Rc<str>,
    pub name_and_type: NameAndType<Descriptor>,
}

pub struct EnclosingMethod {
    pub class: Rc<str>,
    pub method: Option<NameAndType<MethodDescriptor>>,
}

pub struct NameAndType<Descriptor> {
    pub name: Rc<str>,
    pub descriptor: Descriptor,
}

pub struct InnerClass {
    pub inner_class: Rc<str>,
    pub visibility: Visibility,
    pub outer_class: Option<Rc<str>>,
    pub inner_name: Option<Rc<str>>,
    pub flags: InnerClassFlags,
}

bitflags! {
    pub struct InnerClassFlags: u8 {
        const STATIC = 1 << 0;
        const FINAL = 1 << 1;
        const INTERFACE = 1 << 2;
        const ABSTRACT = 1 << 3;
        const SYNTHETIC = 1 << 4;
        const ANNOTATION = 1 << 5;
        const ENUM = 1 << 6;
    }
}

pub enum Constant {
    String(Rc<str>),
    Int(u32),
    Float(f32),
    Long(u64),
    Double(f64),
    // TODO: other constant variants...
}

pub struct Member<Descriptor, TypeAnnotationTarget> {
    pub name: Rc<str>,
    pub visibility: Visibility,
    pub descriptor: Descriptor,
    pub signature: Option<Rc<str>>,
    pub runtime_visible_annotations: Rc<[Annotation]>,
    pub runtime_invisible_annotations: Rc<[Annotation]>,
    pub runtime_visible_type_annotations: Rc<[TypeAnnotation<TypeAnnotationTarget>]>,
    pub runtime_invisible_type_annotations: Rc<[TypeAnnotation<TypeAnnotationTarget>]>,
    pub attributes: Rc<[Rc<str>]>,
}

pub enum Mutability {
    Final,
    Mutable,
    Volatile,
}

pub struct TypeAnnotation<Target> {
    pub target: Target,
    pub target_path: Rc<[TypePathElement]>,
}

pub struct Annotation {
    pub descriptor: FieldDescriptor,
    pub element_value_pairs: Rc<[ElementValuePair]>,
}

pub struct ElementValuePair {
    pub name: Rc<str>,
    pub value: ElementValue,
}

pub enum ElementValue {
    Byte(u8),
    Char(u8),
    Double(f64),
    Float(f32),
    int(i32),
    Long(i64),
    Short(i16),
    String(Rc<str>),
    Enum {
        descriptor: FieldDescriptor,
        name: Rc<str>,
    },
    Class(Option<FieldDescriptor>),
    Annotation(Annotation),
    Array(Rc<[Self]>),
}

pub enum Visibility {
    Private,
    Protected,
    Public,
}

pub struct Method {
    pub member: Member<MethodDescriptor, MethodTypeAnnotationTarget>,
    pub code: Option<Code>,
    pub throws: Rc<[Rc<str>]>,
    pub runtime_visible_parameter_annotations: Rc<[Annotation]>,
    pub runtime_invisible_parameter_annotations: Rc<[Annotation]>,
    pub annotation_default: Option<ElementValue>,
    pub parameters: Rc<[Parameter]>,
}

pub struct Parameter {
    pub name: Rc<str>,
    pub flags: ParameterFlags,
}

bitflags! {
    pub struct ParameterFlags: u8 {
        const FINAL = 1 << 0;
        const SYNTHETIC = 1 << 1;
        const MANDATED = 1 << 2;
    }
}

pub struct Code {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: Rc<[u8]>,
    pub exception_handlers: Rc<[ExceptionHandler]>,
    pub stack_map_frames: Rc<[StackMapFrame]>,
    pub line_numbers: Rc<[LineNumber]>,
    pub local_variables: Rc<[LocalVariable]>,
    pub local_variable_types: Rc<[LocalVariableType]>,
    pub attributes: Rc<[Rc<str>]>,
}

pub struct LocalVariableType {
    pub scope: Range<usize>,
    pub name: Rc<str>,
    pub descriptor: FieldDescriptor,
    pub variable_index: usize,
}

pub struct LocalVariable {
    pub scope: Range<usize>,
    pub name: Rc<str>,
    pub descriptor: FieldDescriptor,
    pub variable_index: usize,
}

pub struct StackMapFrame {
    pub offset_delta: u16,
    pub info: StackMapFrameInfo,
}

pub enum StackMapFrameInfo {
    Same,
    SameLocals1StackItem {
        stack: VerificationTypeInfo,
    },
    Chop {
        removed: u8,
    },
    Append {
        locals: Rc<[VerificationTypeInfo]>,
    },
    Full {
        locals: Rc<[VerificationTypeInfo]>,
        stack: Rc<[VerificationTypeInfo]>,
    },
}

pub enum VerificationTypeInfo {
    Top,
    Integer,
    Float,
    Double,
    Long,
    Null,
    UninitializedThis,
    Object(Rc<str>),
    Uninitialized { offset: usize },
}

pub struct ExceptionHandler {
    pub active: Range<usize>,
    pub handler_pc: usize,
    pub catch_type: Option<Rc<str>>,
}

// TODO: make sure none of these conflict
bitflags! {
    pub struct MethodFlags: u16 {
        const STATIC = 1 << 0;
        const FINAL = 1 << 1;
        const SYNCHRONIZED = 1 << 2;
        const BRIDGE = 1 << 3;
        const VARARGS = 1 << 4;
        const NATIVE = 1 << 5;
        const ABSTRACT = 1 << 6;
        const STRICT = 1 << 7;
        const SYNTHETIC = 1 << 8;
    }
}

pub struct Field {
    pub member: Member<FieldDescriptor, FieldTypeAnnotationTarget>,
    pub flags: FieldFlags,
    pub constant_value: Option<Rc<Constant>>,
}

impl Field {
    pub fn is_static(&self) -> bool {
        self.flags.contains(FieldFlags::STATIC)
    }

    pub fn is_transient(&self) -> bool {
        self.flags.contains(FieldFlags::TRANSIENT)
    }

    pub fn is_synthetic(&self) -> bool {
        self.flags.contains(FieldFlags::SYNTHETIC)
    }

    pub fn is_enum(&self) -> bool {
        self.flags.contains(FieldFlags::ENUM)
    }

    pub fn is_deprecated(&self) -> bool {
        self.flags.contains(FieldFlags::DEPRECATED)
    }
}

bitflags! {
    pub struct FieldFlags: u8 {
        const STATIC = 1 << 0;
        const TRANSIENT = 1 << 1;
        const SYNTHETIC = 1 << 2;
        const ENUM = 1 << 3;
        const DEPRECATED = 1 << 4;
    }
}
