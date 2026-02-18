use super::{Parsed, cp_info, u1, u2, u4};
use bitflags::bitflags;
use std::{io::Read, ops::Range, rc::Rc};

#[derive(Debug)]
// TODO: only certain attributes are allowed in certain contexts
pub enum attribute_info {
    /// If `ACC_STATIC` is set, then the field represented by the parent [`field_info`] is assigned
    /// the value at `constantvalue_index` as part of the initialization of the class or interface
    /// declaring the field.
    ///
    /// There can be at most one `ConstantValue` attribute per field.
    ///
    /// Only located in [`field_info`](super::field_info).
    ConstantValue { constantvalue_index: usize },
    /// Only located in [`method_info`](super::method_info.
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
        attributes: Rc<[Self]>,
    },

    /// Used for type checking.
    ///
    /// For ^v50.0, if `Code` doesn't have a `StackMapTable`, an empty table is implied.
    ///
    /// There can be at most one `StackMapTable` per `Code`.
    ///
    /// Only located in [`Code`](Self::Code)
    StackMapTable(Rc<[stack_map_frame]>),

    /// Indicates which checked exceptions a method may throw.
    ///
    /// There can be at most one `Exceptions` attribute per method.
    ///
    /// Only located in [`method_info`](super::method_info)
    Exceptions {
        /// Indices into the constant pool at which [`cp_info::Class`]es are found representing the
        /// exceptions thrown by this method.
        exception_index_table: Rc<[usize]>,
    },

    /// Lists inner classes.
    ///
    /// Each [`cp_info::Class`] in the constant pool must have one corresponding entry in the
    /// inner classes table.
    ///
    /// Only located in [`ClassFile`](super::ClassFile)
    InnerClasses(Rc<[InnerClass]>),

    /// A class can only have an `EnclosingMethod` if it represents a local or anonymous class.
    ///
    /// There can be at most one `EnclosingMethod` attribute per class.
    ///
    /// Only located in [`ClassFile`](super::ClassFile)
    EnclosingMethod {
        /// Index into the constant pool at which a [`cp_info::Class`] is found representing an
        /// enclosing class.
        class_index: usize,
        /// Index into the constant pool at which a [`cp_info::NameAndType`] is found representing
        /// the name and descriptor of a method.
        method_index: Option<usize>,
    },

    /// Indicates a class member that does not appear in the source code.
    ///
    /// Located in:
    /// - [`ClassFile`](super::ClassFile)
    /// - [`field_info`](super::field_info)
    /// - [`method_info`](super::method_info)
    Synthetic,

    // TODO: actually figure out how signatures work
    //
    /// There can be at most one `Signature` attribute in a structure.
    ///
    /// Located in:
    /// - [`ClassFile`](super::ClassFile)
    /// - [`field_info`](super::field_info)
    /// - [`method_info`](super::method_info)
    /// - [`record_component_info`]
    Signature {
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a
        /// signature.
        signature_index: usize,
    },

    /// There can be at most one `SourceFile` attribute per class.
    ///
    /// Only located in [`ClassFile`](super::ClassFile)
    SourceFile {
        /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a
        /// source file name.
        sourcefile_index: usize,
    },

    /// Extra debugging information that is not semantically relevant.
    ///
    /// There can be at most one `SourceDebugExtension` attribute per class.
    ///
    /// Only located in [`ClassFile`](super::ClassFile)
    SourceDebugExtension(Rc<str>),

    /// Used to map instructions to lines of source code.
    ///
    /// Only located in [`Code`](Self::Code)
    LineNumberTable(Rc<[LineNumber]>),

    /// Used to map local variables to their names.
    ///
    /// There can be no more than one [`LocalVariable`] per local variable.
    ///
    /// Only located in [`Code`](Self::Code)
    LocalVariableTable(Rc<[LocalVariable]>),

    /// There can be no more than one [`LocalVariableType`] per local variable.
    ///
    /// Only located in [`Code`](Self::Code)
    LocalVariableTypeTable(Rc<[LocalVariableType]>),
}

#[derive(Debug)]
pub struct LocalVariable {
    /// Range of indices in the code array within which the local variable has a value.
    scope: Range<usize>,
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing a name of
    /// a local variable.
    name_index: usize,
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing the field
    /// descriptor of a local variable.
    descriptor_index: usize,
    /// Index into the current frame's local variable array at which a local variable is located.
    index: usize,
}

#[derive(Debug)]
pub struct LocalVariableType {
    /// Range of indices in the code array within which the local variable has a value.
    scope: Range<usize>,
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing the
    /// unqualified name of a local variable.
    name_index: usize,
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing the field
    /// descriptor of a local variable's type.
    descriptor_index: usize,
    /// Index into the current frame's local variable array at which a local variable is located.
    index: usize,
}

#[derive(Debug)]
pub struct LineNumber {
    /// Index of the first instruction of the line in the code array.
    start_pc: usize,
    // TODO: should this be usize?
    line_number: u2,
}

impl Parsed for LineNumber {
    fn parse(mut reader: impl Read) -> std::io::Result<Self> {
        Ok(Self {
            start_pc: usize::parse(&mut reader)?,
            line_number: u2::parse(&mut reader)?,
        })
    }
}

impl attribute_info {
    pub fn parse(mut reader: impl Read, constant_pool: &[cp_info]) -> std::io::Result<Self> {
        use std::io::{Error, ErrorKind};

        let name_index = usize::parse(&mut reader)? - 1;
        let name = match constant_pool.get(name_index).ok_or_else(|| {
            Error::new(
                ErrorKind::InvalidData,
                format!("attribute name index {name_index} is out of bounds"),
            )
        })? {
            cp_info::Utf8(s) => s,
            constant => {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    format!(
                        "constant at index {name_index} for attribute is not Utf8; found: {constant:?}"
                    ),
                ));
            }
        };

        let attribute_length = {
            let mut u4 = [0; 4];
            reader.read_exact(&mut u4)?;
            u4::from_be_bytes(u4)
        };

        match name.as_ref() {
            "ConstantValue" => Ok(Self::ConstantValue {
                constantvalue_index: usize::parse(&mut reader)? - 1,
            }),
            "Code" => {
                let mut u2 = [0; 2];

                reader.read_exact(&mut u2)?;
                let max_stack = u2::from_be_bytes(u2);

                reader.read_exact(&mut u2)?;
                let max_locals = u2::from_be_bytes(u2);

                let mut code_length = [0; 4];
                reader.read_exact(&mut code_length)?;
                let code = (0..u4::from_be_bytes(code_length))
                    .map(|_| u1::parse(&mut reader))
                    .collect::<Result<_, _>>()?;

                let exception_table = (0..u2::parse(&mut reader)?)
                    .map(|_| ExceptionHandler::parse(&mut reader))
                    .collect::<Result<_, _>>()?;

                let attributes = (0..u2::parse(&mut reader)?)
                    .map(|_| Self::parse(&mut reader, constant_pool))
                    .collect::<Result<_, _>>()?;

                Ok(Self::Code {
                    max_stack,
                    max_locals,
                    code,
                    exception_table,
                    attributes,
                })
            }
            "StackMapTable" => Ok(Self::StackMapTable(
                (0..u2::parse(&mut reader)?)
                    .map(|_| stack_map_frame::parse(&mut reader))
                    .collect::<Result<_, _>>()?,
            )),
            "Exceptions" => Ok(Self::Exceptions {
                exception_index_table: (0..u2::parse(&mut reader)?)
                    .map(|_| usize::parse(&mut reader))
                    .collect::<Result<_, _>>()?,
            }),
            "InnerClasses" => Ok(Self::InnerClasses(
                (0..u2::parse(&mut reader)?)
                    .map(|_| InnerClass::parse(&mut reader))
                    .collect::<Result<_, _>>()?,
            )),
            "EnclosingMethod" => Ok(Self::EnclosingMethod {
                class_index: usize::parse(&mut reader)? - 1,
                method_index: match usize::parse(&mut reader)? {
                    0 => None,
                    i => Some(i - 1),
                },
            }),
            "Synthetic" => Ok(Self::Synthetic),
            "Signature" => Ok(Self::Signature {
                signature_index: usize::parse(&mut reader)? - 1,
            }),
            "SourceFile" => Ok(Self::SourceFile {
                sourcefile_index: usize::parse(&mut reader)? - 1,
            }),
            "SourceDebugExtension" => Ok(Self::SourceDebugExtension(
                std::io::read_to_string((&mut reader).take(attribute_length as u64))?.into(),
            )),
            "LineNumberTable" => Ok(Self::LineNumberTable(
                (0..u2::parse(&mut reader)?)
                    .map(|_| LineNumber::parse(&mut reader))
                    .collect::<Result<_, _>>()?,
            )),
            "LocalVariableTable" => todo!(),
            "LocalVariableTypeTable" => todo!(),
            "Deprecated" => todo!(),
            "RuntimeVisibleAnnotations" => todo!(),
            "RuntimeInvisibleAnnotations" => todo!(),
            "RuntimeVisibleParameterAnnotations" => todo!(),
            "RuntimeInvisibleParameterAnnotations" => todo!(),
            "RuntimeVisibleTypeAnnotations" => todo!(),
            "RuntimeInvisibleTypeAnnotations" => todo!(),
            "AnnotationDefault" => todo!(),
            "BootstrapMethods" => todo!(),
            "MethodParameters" => todo!(),
            "Module" => todo!(),
            "ModulePackages" => todo!(),
            "ModuleMainClass" => todo!(),
            "NestHost" => todo!(),
            "NestMembers" => todo!(),
            "Record" => todo!(),
            "PermittedSubclasses" => todo!(),
            _ => todo!("unknown"),
        }
    }
}

#[derive(Debug)]
pub struct InnerClass {
    /// Index into the constant pool at which a [`cp_info::Class`] is found representing the
    /// inner class.
    inner_class_info_index: usize,
    /// Index into the constant pool at which a [`cp_info::Class`] is found representing the outer
    /// class, if any.
    ///
    /// Must be `None` if anonymous for ^v51.0.
    outer_class_info_index: Option<usize>,
    /// Index into the constant pool at which a [`cp_info::Utf8`] is found representing the simple
    /// name of the inner class.
    ///
    /// `None` if anonymous.
    inner_name_index: Option<usize>,
    inner_class_access_flags: InnerClassAccessFlags,
}

impl Parsed for InnerClass {
    fn parse(mut reader: impl Read) -> std::io::Result<Self> {
        let mut u2 = [0; 2];

        reader.read_exact(&mut u2)?;
        let inner_class_info_index = u2::from_be_bytes(u2) as usize - 1;

        reader.read_exact(&mut u2)?;
        let outer_class_info_index = if u2::from_be_bytes(u2) == 0 {
            None
        } else {
            Some(u2::from_be_bytes(u2) as usize - 1)
        };

        reader.read_exact(&mut u2)?;
        let inner_name_index = if u2::from_be_bytes(u2) == 0 {
            None
        } else {
            Some(u2::from_be_bytes(u2) as usize - 1)
        };

        let inner_class_access_flags = InnerClassAccessFlags::parse(&mut reader)?;

        Ok(Self {
            inner_class_info_index,
            outer_class_info_index,
            inner_name_index,
            inner_class_access_flags,
        })
    }
}

bitflags! {
    #[derive(Debug)]
    struct InnerClassAccessFlags: u2 {
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

impl Parsed for InnerClassAccessFlags {
    fn parse(mut reader: impl Read) -> std::io::Result<Self> {
        use std::io::{Error, ErrorKind};

        let mut u2 = [0; 2];
        reader.read_exact(&mut u2)?;
        InnerClassAccessFlags::from_bits(u2::from_be_bytes(u2)).ok_or(Error::new(
            ErrorKind::InvalidData,
            "invalid inner class access_flags",
        ))
    }
}

#[derive(Debug)]
pub struct stack_map_frame {
    offset_delta: u2,
    info: stack_map_frame_info,
}

impl Parsed for stack_map_frame {
    fn parse(mut reader: impl Read) -> std::io::Result<Self> {
        use std::io::{Error, ErrorKind};

        let tag = u1::parse(&mut reader)?;

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
                offset_delta: u2::parse(&mut reader)?,
                info: stack_map_frame_info::same_locals_1_stack_item_frame {
                    stack: verification_type_info::parse(reader)?,
                },
            }),
            248..=250 => Ok(Self {
                offset_delta: u2::parse(&mut reader)?,
                info: stack_map_frame_info::chop_frame { removed: 251 - tag },
            }),
            251 => Ok(Self {
                offset_delta: u2::parse(&mut reader)?,
                info: stack_map_frame_info::same_frame,
            }),
            252..=254 => Ok(Self {
                offset_delta: u2::parse(&mut reader)?,
                info: stack_map_frame_info::append_frame {
                    locals: (0..(tag - 251))
                        .map(|_| verification_type_info::parse(&mut reader))
                        .collect::<Result<_, _>>()?,
                },
            }),
            255 => Ok(Self {
                offset_delta: u2::parse(&mut reader)?,
                info: stack_map_frame_info::full_frame {
                    locals: (0..u2::parse(&mut reader)?)
                        .map(|_| verification_type_info::parse(&mut reader))
                        .collect::<Result<_, _>>()?,
                    stack: (0..u2::parse(&mut reader)?)
                        .map(|_| verification_type_info::parse(&mut reader))
                        .collect::<Result<_, _>>()?,
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

#[derive(Debug)]
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
        cpool_index: usize,
    },
    Uninitialized {
        /// Offset in the code array of the `new` instruction that created the object.
        offset: u2,
    },
}

impl Parsed for verification_type_info {
    fn parse(mut reader: impl Read) -> std::io::Result<Self> {
        use std::io::{Error, ErrorKind};

        let tag = u1::parse(&mut reader)?;

        match tag {
            0 => Ok(Self::Top),
            1 => Ok(Self::Integer),
            2 => Ok(Self::Float),
            3 => Ok(Self::Double),
            4 => Ok(Self::Long),
            5 => Ok(Self::Null),
            6 => Ok(Self::UninitializedThis),
            7 => Ok(Self::Object {
                cpool_index: usize::parse(&mut reader)? - 1,
            }),
            8 => Ok(Self::Uninitialized {
                offset: u2::parse(&mut reader)?,
            }),
            _ => Err(Error::new(
                ErrorKind::InvalidData,
                format!("invalid verification type tag: {tag}"),
            )),
        }
    }
}

#[derive(Debug)]
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
    catch_type: Option<usize>,
}

impl Parsed for ExceptionHandler {
    fn parse(mut reader: impl Read) -> std::io::Result<Self> {
        let mut u2 = [0; 2];

        reader.read_exact(&mut u2)?;
        let start_pc = u2::from_be_bytes(u2) as usize;

        reader.read_exact(&mut u2)?;
        let end_pc = u2::from_be_bytes(u2) as usize;

        let active = start_pc..end_pc;

        reader.read_exact(&mut u2)?;
        let handler_pc = u2::from_be_bytes(u2);

        reader.read_exact(&mut u2)?;
        let catch_type = if u2::from_be_bytes(u2) == 0 {
            None
        } else {
            Some(u2::from_be_bytes(u2) as usize - 1)
        };

        Ok(Self {
            active,
            handler_pc,
            catch_type,
        })
    }
}
