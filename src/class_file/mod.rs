mod raw;

use crate::{
    class_file::raw::attributes::ClassTypeAnnotationTarget,
    descriptors::{
        FieldDescriptor, MethodDescriptor, parse_field_descriptor, parse_method_descriptor,
        parse_return_descriptor,
    },
};
use bitflags::bitflags;
use leche_parse::Parsed;
use raw::{
    MemberAccessFlags, MethodAccessFlags,
    attributes::{
        ClassAttribute, CodeAttribute, FieldAttribute, InnerClassAccessFlags, MemberAttribute,
        MethodAttribute, ParameterAccessFlags, annotation, element_value, stack_map_frame,
        stack_map_frame_info, type_annotation, verification_type_info,
    },
    cp_info,
};
use regex::Regex;
use std::{
    io::{Error, ErrorKind, Read},
    ops::Range,
    rc::Rc,
};

pub use raw::{
    ClassAccessFlags as ClassFlags, ReferenceKind,
    attributes::{FieldTypeAnnotationTarget, LineNumber, MethodTypeAnnotationTarget},
};

#[derive(Debug)]
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
    pub signature: Option<Rc<str>>,
    pub runtime_visible_annotations: Rc<[Annotation]>,
    pub runtime_invisible_annotations: Rc<[Annotation]>,
    pub runtime_visible_type_annotations: Rc<[TypeAnnotation<ClassTypeAnnotationTarget>]>,
    pub runtime_invisible_type_annotations: Rc<[TypeAnnotation<ClassTypeAnnotationTarget>]>,
    pub attributes: Rc<[Rc<str>]>,
}

impl ClassFile {
    pub fn new(mut input: impl Read) -> std::io::Result<Self> {
        let raw @ raw::ClassFile {
            major_version,
            minor_version,
            ..
        } = raw::ClassFile::parse(&mut input)?;

        if input.read(&mut [0; 2])? != 0 {
            return Err(Error::new(
                ErrorKind::InvalidData,
                "invalid class file: extra bytes at end of file",
            ));
        }

        if !(45..=69).contains(&major_version) {
            return Err(Error::new(
                ErrorKind::InvalidData,
                format!("unsupported major version: {major_version}"),
            ));
        }

        if major_version >= 56 && minor_version != 0 && minor_version != u16::MAX {
            return Err(Error::new(
                ErrorKind::InvalidData,
                format!("invalid minor version: {minor_version}"),
            ));
        }

        let this = raw.get_class(raw.this_class)?.clone();

        let superclass = raw
            .super_class
            .map(|i| raw.get_class(i).map(Rc::clone))
            .transpose()?;

        let interfaces = raw
            .interfaces
            .iter()
            .map(|i| raw.get_class(*i).map(Rc::clone))
            .collect::<Result<_, _>>()?;

        let fields = raw
            .fields
            .iter()
            .map(|f| Self::make_field(&raw, f))
            .collect::<Result<_, _>>()?;

        let methods = raw
            .methods
            .iter()
            .map(|m| Self::make_method(&raw, m))
            .collect::<Result<_, _>>()?;

        let constants = raw
            .constant_pool
            .iter()
            .map(|c| Self::make_constant(&raw, c))
            .collect::<Result<_, _>>()?;

        let (
            mut source_file,
            mut inner_classes,
            mut enclosing_method,
            mut source_debug_extension,
            mut bootstrap_methods,
            mut module,
            mut module_packages,
            mut module_main_class,
            mut nest,
            mut record,
            mut permitted_subclasses,
        ) = (
            None, None, None, None, None, None, None, None, None, None, None,
        );

        let member = Self::make_member(
            &raw,
            raw.attributes.iter().filter_map(|a| {
                match a {
                    ClassAttribute::SourceFile { sourcefile_index } => {
                        if source_file.is_some() {
                            source_file = Some(Err(Error::new(
                                ErrorKind::InvalidData,
                                "class has more than one SourceFile attribute",
                            )));
                        } else {
                            source_file = Some(raw.get_utf8(*sourcefile_index).map(Rc::clone));
                        }
                    }
                    ClassAttribute::InnerClasses(classes) => {
                        if inner_classes.is_some() {
                            inner_classes = Some(Err(Error::new(
                                ErrorKind::InvalidData,
                                "class has more than one InnerClasses attribute",
                            )));
                        } else {
                            inner_classes = Some(
                                classes
                                    .iter()
                                    .map(|i| {
                                        Ok(InnerClass {
                                            inner_class: raw
                                                .get_class(i.inner_class_info_index)?
                                                .clone(),
                                            visibility: i.inner_class_access_flags.into(),
                                            flags: i.inner_class_access_flags.into(),
                                            inner_name: i
                                                .inner_name_index
                                                .map(|i| raw.get_utf8(i).map(Rc::clone))
                                                .transpose()?,
                                            outer_class: i
                                                .outer_class_info_index
                                                .map(|i| raw.get_class(i).map(Rc::clone))
                                                .transpose()?,
                                        })
                                    })
                                    .collect(),
                            )
                        }
                    }
                    ClassAttribute::EnclosingMethod {
                        class_index,
                        method_index,
                    } => {
                        if enclosing_method.is_some() {
                            enclosing_method = Some(Err(Error::new(
                                ErrorKind::InvalidData,
                                "class has more than one EnclosingMethod attribute",
                            )));
                        } else {
                            enclosing_method = Some(
                                raw.get_class(*class_index)
                                    .map(Rc::clone)
                                    .and_then(|class| {
                                        method_index
                                            .as_ref()
                                            .map(|i| {
                                                Self::make_name_and_type(&raw, *i, |i| {
                                                    Self::get_method_descriptor(&raw, i)
                                                })
                                            })
                                            .transpose()
                                            .map(|method| (class, method))
                                    })
                                    .map(|(class, method)| EnclosingMethod { class, method }),
                            );
                        }
                    }
                    ClassAttribute::SourceDebugExtension(debug_extension) => {
                        if source_debug_extension.is_some() {
                            source_debug_extension = Some(Err(Error::new(
                                ErrorKind::InvalidData,
                                "class has more than one SourceDebugExtension attribute",
                            )));
                        } else {
                            source_debug_extension = Some(Ok(debug_extension.clone()));
                        }
                    }
                    ClassAttribute::BootstrapMethods(methods) => {
                        if bootstrap_methods.is_some() {
                            bootstrap_methods = Some(Err(Error::new(
                                ErrorKind::InvalidData,
                                "class has more than one BootstrapMethods attribute",
                            )));
                        } else {
                            bootstrap_methods = Some(
                                methods
                                    .iter()
                                    .map(|m| {
                                        let info = raw.get_method_handle(m.bootstrap_method_ref)?;
                                        Ok(BootstrapMethod {
                                            method: Ref {
                                                class: raw.get_class(info.class_index)?.clone(),
                                                name_and_type: Self::make_name_and_type(
                                                    &raw,
                                                    info.name_and_type_index,
                                                    |i| Self::get_method_descriptor(&raw, i),
                                                )?,
                                            },
                                            arguments: m
                                                .bootstrap_arguments
                                                .iter()
                                                .map(|i| {
                                                    Self::make_constant(
                                                        &raw,
                                                        &raw.constant_pool[*i],
                                                    )
                                                })
                                                .collect::<Result<_, _>>()?,
                                        })
                                    })
                                    .collect(),
                            );
                        }
                    }
                    ClassAttribute::Module(m) => {
                        if module.is_some() {
                            module = Some(Err(Error::new(
                                ErrorKind::InvalidData,
                                "class has more than one Module attribute",
                            )));
                        } else {
                            module = Some(
                                raw.get_utf8(m.module_name_index)
                                    .map(|name| (name.clone(), m.module_flags.into()))
                                    .and_then(|(name, flags)| {
                                        Ok((
                                            name,
                                            flags,
                                            m.module_version_index
                                                .map(|i| raw.get_utf8(i).map(Rc::clone))
                                                .transpose()?,
                                        ))
                                    })
                                    .and_then(|(name, flags, version)| {
                                        m.requires
                                            .iter()
                                            .map(|r| {
                                                Ok(Require {
                                                    name: raw.get_module(r.requires_index)?.clone(),
                                                    flags: r.requires_flags.into(),
                                                    version: r
                                                        .requires_version_index
                                                        .map(|i| raw.get_utf8(i).map(Rc::clone))
                                                        .transpose()?,
                                                })
                                            })
                                            .collect::<Result<_, _>>()
                                            .map(|requires| (name, flags, version, requires))
                                    })
                                    .and_then(|(name, flags, version, requires)| {
                                        m.exports
                                            .iter()
                                            .map(|e| Self::make_export(&raw, e))
                                            .collect::<Result<_, _>>()
                                            .map(|exports| {
                                                (name, flags, version, requires, exports)
                                            })
                                    })
                                    .and_then(|(name, flags, version, requires, exports)| {
                                        m.opens
                                            .iter()
                                            .map(|e| Self::make_export(&raw, e))
                                            .collect::<Result<_, _>>()
                                            .map(|opens| {
                                                (name, flags, version, requires, exports, opens)
                                            })
                                    })
                                    .and_then(|(name, flags, version, requires, exports, opens)| {
                                        m.uses_index
                                            .0
                                            .iter()
                                            .map(|i| raw.get_class(*i).map(Rc::clone))
                                            .collect::<Result<_, _>>()
                                            .map(|uses| {
                                                (
                                                    name, flags, version, requires, exports, opens,
                                                    uses,
                                                )
                                            })
                                    })
                                    .and_then(
                                        |(
                                            name,
                                            flags,
                                            module_version,
                                            requires,
                                            exports,
                                            opens,
                                            uses,
                                        )| {
                                            m.provides
                                                .iter()
                                                .map(|p| {
                                                    Ok(Provide {
                                                        class: raw
                                                            .get_class(p.provides_index)?
                                                            .clone(),
                                                        provides_with: p
                                                            .provides_with_index
                                                            .iter()
                                                            .map(|i| {
                                                                raw.get_class(*i).map(Rc::clone)
                                                            })
                                                            .collect::<Result<_, _>>()?,
                                                    })
                                                })
                                                .collect::<Result<_, _>>()
                                                .map(|provides| Module {
                                                    name,
                                                    flags,
                                                    module_version,
                                                    requires,
                                                    exports,
                                                    opens,
                                                    uses,
                                                    provides,
                                                })
                                        },
                                    ),
                            )
                        }
                    }
                    ClassAttribute::ModulePackages(packages) => {
                        if module_packages.is_some() {
                            module_packages = Some(Err(Error::new(
                                ErrorKind::InvalidData,
                                "class has more than one ModulePackages attribute",
                            )));
                        } else {
                            module_packages = Some(
                                packages
                                    .iter()
                                    .map(|i| raw.get_package(*i).map(Rc::clone))
                                    .collect(),
                            );
                        }
                    }
                    ClassAttribute::ModuleMainClass(index) => {
                        if module_main_class.is_some() {
                            module_main_class = Some(Err(Error::new(
                                ErrorKind::InvalidData,
                                "class has more than one ModuleMainClass attribute",
                            )));
                        } else {
                            module_main_class = Some(raw.get_class(*index).map(Rc::clone));
                        }
                    }
                    ClassAttribute::NestHost(index) => {
                        if let Some(nest_value) = &nest {
                            let message = match nest_value {
                                Ok(Nest::Host(_)) => "class has more than one NestHost attribute",
                                Ok(Nest::Members(_)) => {
                                    "class has both NestHost and NestMembers attributes"
                                }
                                Err(_) => return None,
                            };
                            nest = Some(Err(Error::new(ErrorKind::InvalidData, message)));
                        } else {
                            nest = Some(raw.get_class(*index).map(Rc::clone).map(Nest::Host));
                        }
                    }
                    ClassAttribute::NestMembers(members) => {
                        if let Some(nest_value) = &nest {
                            let message = match nest_value {
                                Ok(Nest::Members(_)) => {
                                    "class has more than one NestMembers attribute"
                                }
                                Ok(Nest::Host(_)) => {
                                    "class has both NestHost and NestMembers attributes"
                                }
                                Err(_) => return None,
                            };
                            nest = Some(Err(Error::new(ErrorKind::InvalidData, message)));
                        } else {
                            nest = Some(
                                members
                                    .iter()
                                    .map(|i| raw.get_class(*i).map(Rc::clone))
                                    .collect::<Result<_, _>>()
                                    .map(Nest::Members),
                            );
                        }
                    }
                    ClassAttribute::Record(records) => {
                        if record.is_some() {
                            record = Some(Err(Error::new(
                                ErrorKind::InvalidData,
                                "class has more than one Record attribute",
                            )));
                        } else {
                            bitflags! {
                                #[derive(Clone, Copy)]
                                struct BlankFlags: u8 {}
                            }
                            record = Some(
                                records
                                    .iter()
                                    .map(|r| {
                                        Self::make_member(
                                            &raw,
                                            r.attributes.iter(),
                                            raw.get_utf8(r.name_index)?.clone(),
                                            Self::get_field_descriptor(&raw, r.descriptor_index)?,
                                            ().into(),
                                            &mut BlankFlags::empty(),
                                            BlankFlags::empty(),
                                            BlankFlags::empty(),
                                        )
                                    })
                                    .collect(),
                            );
                        }
                    }
                    ClassAttribute::PermittedSubclasses(subclasses) => {
                        if permitted_subclasses.is_some() {
                            permitted_subclasses = Some(Err(Error::new(
                                ErrorKind::InvalidData,
                                "class has more than one PermittedSubclasses attribute",
                            )));
                        } else {
                            permitted_subclasses = Some(
                                subclasses
                                    .iter()
                                    .map(|i| raw.get_class(*i).map(Rc::clone))
                                    .collect(),
                            );
                        }
                    }
                    ClassAttribute::Member(m) => return Some(m),
                }
                None
            }),
            Rc::default(),
            (),
            Visibility::Package,
            &mut MethodFlags::empty(),
            MethodFlags::empty(),
            MethodFlags::empty(),
        )?;

        Ok(Self {
            major_version,
            minor_version,
            constants,
            flags: raw.access_flags,
            this,
            superclass,
            interfaces,
            fields,
            methods,
            source_file: source_file.transpose()?,
            inner_classes: inner_classes.transpose()?.unwrap_or_default(),
            enclosing_method: enclosing_method.transpose()?,
            source_debug_extension: source_debug_extension.transpose()?,
            bootstrap_methods: bootstrap_methods.transpose()?.unwrap_or_default(),
            module: module.transpose()?,
            module_packages: module_packages.transpose()?.unwrap_or_default(),
            module_main_class: module_main_class.transpose()?,
            nest: nest.transpose()?,
            record: record.transpose()?,
            permitted_subclasses: permitted_subclasses.transpose()?,
            signature: member.signature,
            runtime_visible_annotations: member.runtime_visible_annotations,
            runtime_invisible_annotations: member.runtime_invisible_annotations,
            runtime_visible_type_annotations: member.runtime_visible_type_annotations,
            runtime_invisible_type_annotations: member.runtime_invisible_type_annotations,
            attributes: member.attributes,
        })
    }

    fn make_constant(raw: &raw::ClassFile, info: &cp_info) -> std::io::Result<Constant> {
        match info {
            cp_info::Utf8(s) => Ok(Constant::Str(s.clone())),
            cp_info::Integer(i) => Ok(Constant::Int(*i)),
            cp_info::Float(f) => Ok(Constant::Float(*f)),
            cp_info::Long(l) => Ok(Constant::Long(*l)),
            cp_info::Double(d) => Ok(Constant::Double(*d)),
            cp_info::SecondHalf => Ok(Constant::SecondHalf),
            cp_info::Class { name_index } => {
                Ok(Constant::Class(raw.get_utf8(*name_index)?.clone()))
            }
            cp_info::String { string_index } => {
                Ok(Constant::String(raw.get_utf8(*string_index)?.clone()))
            }
            cp_info::Fieldref(info) => Ok(Constant::Fieldref(Ref {
                class: raw.get_class(info.class_index)?.clone(),
                name_and_type: Self::make_name_and_type(raw, info.name_and_type_index, |i| {
                    Self::get_field_descriptor(raw, i)
                })?,
            })),
            cp_info::Methodref(info) => Ok(Constant::MethodRef(Ref {
                class: raw.get_class(info.class_index)?.clone(),
                name_and_type: Self::make_name_and_type(raw, info.name_and_type_index, |i| {
                    Self::get_method_descriptor(raw, i)
                })?,
            })),
            cp_info::InterfaceMethodref(info) => Ok(Constant::InterfaceMethodRef(Ref {
                class: raw.get_class(info.class_index)?.clone(),
                name_and_type: Self::make_name_and_type(raw, info.name_and_type_index, |i| {
                    Self::get_method_descriptor(raw, i)
                })?,
            })),
            cp_info::NameAndType { .. } => Ok(Constant::NameAndType),
            cp_info::MethodHandle {
                reference_kind,
                reference_index,
            } => {
                let ref_info = raw.get_methodref(*reference_index)?;
                Ok(Constant::MethodHandle {
                    kind: *reference_kind,
                    reference: Ref {
                        class: raw.get_class(ref_info.class_index)?.clone(),
                        name_and_type: Self::make_name_and_type(
                            raw,
                            ref_info.name_and_type_index,
                            |i| Self::get_method_descriptor(raw, i),
                        )?,
                    },
                })
            }
            cp_info::MethodType { descriptor_index } => Ok(Constant::MethodType(
                Self::get_method_descriptor(raw, *descriptor_index)?,
            )),
            cp_info::Dynamic(info) => Ok(Constant::Dynamic(DynamicInfo {
                bootstrap_method_index: info.bootstrap_method_attr_index,
                nat: Self::make_name_and_type(raw, info.name_and_type_index, |i| {
                    Self::get_method_descriptor(raw, i)
                })?,
            })),
            cp_info::InvokeDynamic(info) => Ok(Constant::InvokeDynamic(DynamicInfo {
                bootstrap_method_index: info.bootstrap_method_attr_index,
                nat: Self::make_name_and_type(raw, info.name_and_type_index, |i| {
                    Self::get_method_descriptor(raw, i)
                })?,
            })),
            cp_info::Module { name_index } => {
                let name = raw.get_utf8(*name_index)?;
                if name
                    .chars()
                    .all(|c| !('\u{0000}'..='\u{001f}').contains(&c))
                    && Regex::new(r"^((\\@)|(\\\\)|(\\:)|[^\\@:])*$")
                        .unwrap()
                        .is_match(name)
                {
                    Ok(Constant::Module(name.clone()))
                } else {
                    Err(Error::new(
                        ErrorKind::InvalidData,
                        format!("Invalid module name: {name}"),
                    ))
                }
            }
            cp_info::Package { name_index } => {
                Ok(Constant::Package(raw.get_utf8(*name_index)?.clone()))
            }
        }
    }

    fn make_export(
        raw: &raw::ClassFile,
        info: &raw::attributes::Export,
    ) -> std::io::Result<Export> {
        Ok(Export {
            name: raw.get_package(info.exports_index)?.clone(),
            flags: info.exports_flags.into(),
            exports_to: info
                .exports_to_index
                .iter()
                .map(|i| raw.get_module(*i).map(Rc::clone))
                .collect::<Result<_, _>>()?,
        })
    }

    fn make_name_and_type<Descriptor>(
        raw: &raw::ClassFile,
        index: usize,
        desc_getter: impl Fn(usize) -> std::io::Result<Descriptor>,
    ) -> std::io::Result<NameAndType<Descriptor>> {
        let (name, descriptor_index) = raw.get_name_and_type(index)?;
        let descriptor = desc_getter(descriptor_index)?;
        Ok(NameAndType {
            name: name.clone(),
            descriptor,
        })
    }

    fn make_method(raw: &raw::ClassFile, info: &raw::method_info) -> std::io::Result<Method> {
        let name = raw.get_utf8(info.name_index)?.clone();
        let mut flags = MethodFlags::from(info.access_flags);
        let visibility = info.access_flags.into();

        let (
            mut code,
            mut throws,
            mut runtime_visible_parameter_annotations,
            mut runtime_invisible_parameter_annotations,
            mut annotation_default,
            mut parameters,
        ) = (None, None, None, None, None, None);

        let descriptor = Self::get_method_descriptor(raw, info.descriptor_index)?;

        let member = Self::make_member(
            raw,
            info.attributes.iter().filter_map(|a| {match a {
                MethodAttribute::Code {
                    max_stack,
                    max_locals,
                    code: instructions,
                    exception_table,
                    attributes,
                } => {
                    if code.is_some() {
                        code = Some(Err(Error::new(
                            ErrorKind::InvalidData,
                            "method has more than one code attribute",
                        )));
                    } else {
                        code = Some(Self::make_code(
                            raw,
                            *max_stack,
                            *max_locals,
                            instructions,
                            exception_table,
                            attributes,
                        ));
                    }
                }
                MethodAttribute::Exceptions {exception_index_table} => {
                    if throws.is_some() {
                        throws = Some(Err(Error::new(
                            ErrorKind::InvalidData,
                            "method has more than one Exceptions attribute",
                        )));
                    } else {
                        throws = Some(exception_index_table.iter().map(|i| raw.get_class(*i).map(Rc::clone)).collect());
                    }
                }
                MethodAttribute::RuntimeVisibleParameterAnnotations(annotations) => {
                    if runtime_visible_parameter_annotations.is_some() {
                        runtime_visible_parameter_annotations = Some(Err(Error::new(
                            ErrorKind::InvalidData,
                            "method has more than one RuntimeVisibleParameterAnnotations attribute",
                        )));
                    } else {
                        runtime_visible_parameter_annotations = Some(
                            annotations
                                .iter()
                                .map(|annotations| annotations.iter().map(|a| Self::make_annotation(raw, a)).collect())
                                .collect(),
                        );
                    }
                }
                MethodAttribute::RuntimeInvisibleParameterAnnotations(annotations) => {
                    if runtime_invisible_parameter_annotations.is_some() {
                        runtime_invisible_parameter_annotations = Some(Err(Error::new(
                            ErrorKind::InvalidData,
                            "method has more than one RuntimeInvisibleParameterAnnotations attribute",
                        )));
                    } else {
                        runtime_invisible_parameter_annotations = Some(
                            annotations
                                .iter()
                                .map(|annotations| annotations.iter().map(|a| Self::make_annotation(raw, a)).collect())
                                .collect(),
                        );
                    }
                }
                MethodAttribute::AnnotationDefault(a) => {
                    if annotation_default.is_some() {
                        annotation_default = Some(Err(Error::new(
                            ErrorKind::InvalidData,
                            "method has more than one AnnotationDefault attribute",
                        )));
                    } else {
                        annotation_default = Some(Self::make_element_value(raw, a));
                    }
                }
                MethodAttribute::MethodParameters(a) => {
                    if parameters.is_some() {
                        parameters = Some(Err(Error::new(
                            ErrorKind::InvalidData,
                            "method has more than one MethodParameters attribute",
                        )));
                    } else {
                        parameters = Some(a.iter().map(|p| Ok(Parameter {
                            name: raw.get_utf8(p.name_index)?.clone(),
                            flags: p.access_flags.into(),
                        })).collect());
                    }
                }
                MethodAttribute::Member(a) => return Some(a),
            }
            None}),
            name,
            descriptor,
            visibility,
            &mut flags,
            MethodFlags::SYNTHETIC,
            MethodFlags::DEPRECATED,
        )?;

        Ok(Method {
            member,
            code: code.transpose()?,
            throws: throws.transpose()?.unwrap_or_default(),
            runtime_visible_parameter_annotations: runtime_visible_parameter_annotations
                .transpose()?
                .unwrap_or_default(),
            runtime_invisible_parameter_annotations: runtime_invisible_parameter_annotations
                .transpose()?
                .unwrap_or_default(),
            annotation_default: annotation_default.transpose()?,
            parameters: parameters.transpose()?.unwrap_or_default(),
        })
    }

    fn make_code(
        raw: &raw::ClassFile,
        max_stack: u16,
        max_locals: u16,
        code: &Rc<[u8]>,
        exception_table: &Rc<[raw::attributes::ExceptionHandler]>,
        attributes: &Rc<[CodeAttribute]>,
    ) -> std::io::Result<Code> {
        let (mut stack_map_frames, mut line_numbers, mut local_variables, mut local_variable_types) =
            (None, None, None, None);

        let mut unknown_attributes = Vec::new();

        for a in attributes.iter() {
            match a {
                CodeAttribute::StackMapTable(frames) => {
                    if stack_map_frames.is_some() {
                        return Err(Error::new(
                            ErrorKind::InvalidData,
                            "method has more than one StackMapTable attribute",
                        ));
                    } else {
                        stack_map_frames = Some(
                            frames
                                .iter()
                                .map(|f| Self::make_stack_map_frame(raw, f))
                                .collect::<Result<_, _>>()?,
                        );
                    }
                }
                CodeAttribute::LineNumberTable(numbers) => {
                    if line_numbers.is_some() {
                        return Err(Error::new(
                            ErrorKind::InvalidData,
                            "method has more than one LineNumberTable attribute",
                        ));
                    } else {
                        line_numbers = Some(numbers.clone());
                    }
                }
                CodeAttribute::LocalVariableTable(vars) => {
                    if local_variables.is_some() {
                        return Err(Error::new(
                            ErrorKind::InvalidData,
                            "method has more than one LocalVariableTable attribute",
                        ));
                    } else {
                        local_variables = Some(
                            vars.iter()
                                .map(|v| -> std::io::Result<_> {
                                    Ok(LocalVariable {
                                        scope: v.scope.clone(),
                                        name: raw.get_utf8(v.name_index)?.clone(),
                                        descriptor: Self::get_field_descriptor(
                                            raw,
                                            v.descriptor_index,
                                        )?,
                                        variable_index: v.index,
                                    })
                                })
                                .collect::<Result<_, _>>()?,
                        );
                    }
                }
                CodeAttribute::LocalVariableTypeTable(types) => {
                    if local_variable_types.is_some() {
                        return Err(Error::new(
                            ErrorKind::InvalidData,
                            "method has more than one LocalVariableTypeTable attribute",
                        ));
                    } else {
                        local_variable_types = Some(
                            types
                                .iter()
                                .map(|v| -> std::io::Result<_> {
                                    Ok(LocalVariableType {
                                        scope: v.scope.clone(),
                                        name: raw.get_utf8(v.name_index)?.clone(),
                                        descriptor: Self::get_field_descriptor(
                                            raw,
                                            v.descriptor_index,
                                        )?,
                                        variable_index: v.index,
                                    })
                                })
                                .collect::<Result<_, _>>()?,
                        );
                    }
                }
                CodeAttribute::Unknown(a) => {
                    unknown_attributes.push(a.clone());
                }
            }
        }

        Ok(Code {
            max_stack,
            max_locals,
            code: code.clone(),
            exception_handlers: exception_table
                .iter()
                .map(|h| -> std::io::Result<_> {
                    Ok(ExceptionHandler {
                        active: h.active.clone(),
                        handler_pc: h.handler_pc as usize,
                        catch_type: h
                            .catch_type
                            .as_ref()
                            .map(|i| raw.get_class(*i).map(Rc::clone))
                            .transpose()?,
                    })
                })
                .collect::<Result<_, _>>()?,
            stack_map_frames: stack_map_frames.unwrap_or_default(),
            line_numbers: line_numbers.unwrap_or_default(),
            local_variables: local_variables.unwrap_or_default(),
            local_variable_types: local_variable_types.unwrap_or_default(),
            attributes: unknown_attributes.into(),
        })
    }

    fn make_stack_map_frame(
        raw: &raw::ClassFile,
        frame: &stack_map_frame,
    ) -> std::io::Result<StackMapFrame> {
        Ok(StackMapFrame {
            offset_delta: frame.offset_delta,
            info: match &frame.info {
                stack_map_frame_info::same_frame => StackMapFrameInfo::Same,
                stack_map_frame_info::same_locals_1_stack_item_frame { stack } => {
                    StackMapFrameInfo::SameLocals1StackItem {
                        stack: Self::make_verification_type_info(raw, stack)?,
                    }
                }
                stack_map_frame_info::chop_frame { removed } => {
                    StackMapFrameInfo::Chop { removed: *removed }
                }
                stack_map_frame_info::append_frame { locals } => StackMapFrameInfo::Append {
                    locals: locals
                        .iter()
                        .map(|i| Self::make_verification_type_info(raw, i))
                        .collect::<Result<_, _>>()?,
                },
                stack_map_frame_info::full_frame { locals, stack } => StackMapFrameInfo::Full {
                    locals: locals
                        .iter()
                        .map(|i| Self::make_verification_type_info(raw, i))
                        .collect::<Result<_, _>>()?,
                    stack: stack
                        .iter()
                        .map(|i| Self::make_verification_type_info(raw, i))
                        .collect::<Result<_, _>>()?,
                },
            },
        })
    }

    fn make_verification_type_info(
        raw: &raw::ClassFile,
        info: &verification_type_info,
    ) -> std::io::Result<VerificationTypeInfo> {
        Ok(match info {
            verification_type_info::Top => VerificationTypeInfo::Top,
            verification_type_info::Integer => VerificationTypeInfo::Integer,
            verification_type_info::Float => VerificationTypeInfo::Float,
            verification_type_info::Double => VerificationTypeInfo::Double,
            verification_type_info::Long => VerificationTypeInfo::Long,
            verification_type_info::Null => VerificationTypeInfo::Null,
            verification_type_info::UninitializedThis => VerificationTypeInfo::UninitializedThis,
            verification_type_info::Object { cpool_index } => {
                VerificationTypeInfo::Object(raw.get_class(*cpool_index)?.clone())
            }
            verification_type_info::Uninitialized { offset } => {
                VerificationTypeInfo::Uninitialized {
                    offset: *offset as usize,
                }
            }
        })
    }

    fn make_field(raw: &raw::ClassFile, info: &raw::field_info) -> std::io::Result<Field> {
        let name = raw.get_utf8(info.name_index)?.clone();

        let mut flags = FieldFlags::from(info.access_flags);
        let mutability = if info.access_flags.contains(MemberAccessFlags::ACC_FINAL) {
            Mutability::Final
        } else if info.access_flags.contains(MemberAccessFlags::ACC_VOLATILE) {
            Mutability::Volatile
        } else {
            Mutability::Mutable
        };

        let visibility = info.access_flags.into();

        let descriptor = Self::get_field_descriptor(raw, info.descriptor_index)?;

        let mut constant_value = None;

        let member = Self::make_member(
            raw,
            info.attributes.iter().filter_map(|a| match a {
                FieldAttribute::ConstantValue {
                    constantvalue_index,
                } => {
                    if constant_value.is_some() {
                        constant_value = Some(Err(Error::new(
                            ErrorKind::InvalidData,
                            format!("field {name} has more than one ConstantValue attribute"),
                        )));
                    } else {
                        constant_value = Some(Self::make_constant(
                            raw,
                            &raw.constant_pool[*constantvalue_index],
                        ));
                    }
                    None
                }
                FieldAttribute::Member(a) => Some(a),
            }),
            name.clone(),
            descriptor,
            visibility,
            &mut flags,
            FieldFlags::SYNTHETIC,
            FieldFlags::DEPRECATED,
        )?;

        Ok(Field {
            flags,
            constant_value: constant_value.transpose()?,
            mutability,
            member,
        })
    }

    #[allow(clippy::too_many_arguments, reason = "idrc")]
    fn make_member<
        'a,
        Descriptor,
        TypeAnnotationTarget: Clone + 'a,
        Flags: bitflags::Flags + Copy,
    >(
        raw: &raw::ClassFile,
        attributes: impl IntoIterator<Item = &'a MemberAttribute<TypeAnnotationTarget>>,
        name: Rc<str>,
        descriptor: Descriptor,
        visibility: Visibility,
        flags: &mut Flags,
        synthetic: Flags,
        deprecated: Flags,
    ) -> std::io::Result<Member<Descriptor, TypeAnnotationTarget>> {
        let (
            mut signature,
            mut runtime_visible_annotations,
            mut runtime_invisible_annotations,
            mut runtime_visible_type_annotations,
            mut runtime_invisible_type_annotations,
        ) = (None, None, None, None, None);

        let mut unknown_attributes = Vec::new();

        for a in attributes {
            match a {
                MemberAttribute::Synthetic => {
                    if flags.contains(synthetic) {
                        return Err(Error::new(
                            ErrorKind::InvalidData,
                            format!("field {name} has more than one Synthetic attribute"),
                        ));
                    } else {
                        flags.insert(synthetic);
                    }
                }
                MemberAttribute::Signature { signature_index } => {
                    if signature.is_some() {
                        return Err(Error::new(
                            ErrorKind::InvalidData,
                            format!("field {name} has more than one Signature attribute"),
                        ));
                    } else {
                        signature = Some(raw.get_utf8(*signature_index)?.clone())
                    }
                }
                MemberAttribute::Deprecated => {
                    if flags.contains(deprecated) {
                        return Err(Error::new(
                            ErrorKind::InvalidData,
                            format!("field {name} has more than one Deprecated attribute"),
                        ));
                    } else {
                        flags.insert(deprecated);
                    }
                }
                MemberAttribute::RuntimeVisibleAnnotations(annotations) => {
                    if runtime_visible_annotations.is_some() {
                        return Err(Error::new(
                            ErrorKind::InvalidData,
                            format!(
                                "field {name} has more than one RuntimeVisibleAnnotations attribute"
                            ),
                        ));
                    } else {
                        runtime_visible_annotations = Some(
                            annotations
                                .iter()
                                .map(|a| Self::make_annotation(raw, a))
                                .collect::<Result<_, _>>()?,
                        );
                    }
                }
                MemberAttribute::RuntimeInvisibleAnnotations(annotations) => {
                    if runtime_invisible_annotations.is_some() {
                        if runtime_invisible_annotations.is_some() {
                            return Err(Error::new(
                                ErrorKind::InvalidData,
                                format!(
                                    "field {name} has more than one RuntimeInvisibleAnnotations attribute"
                                ),
                            ));
                        } else {
                            runtime_invisible_annotations = Some(
                                annotations
                                    .iter()
                                    .map(|a| Self::make_annotation(raw, a))
                                    .collect::<Result<_, _>>()?,
                            );
                        }
                    }
                }
                MemberAttribute::RuntimeVisibleTypeAnnotations(annotations) => {
                    if runtime_visible_type_annotations.is_some() {
                        return Err(Error::new(
                            ErrorKind::InvalidData,
                            format!(
                                "field {name} has more than one RuntimeVisibleTypeAnnotations attribute"
                            ),
                        ));
                    } else {
                        runtime_visible_type_annotations =
                            Some(annotations.iter().map(Into::into).collect());
                    }
                }
                MemberAttribute::RuntimeInvisibleTypeAnnotations(annotations) => {
                    if runtime_invisible_type_annotations.is_some() {
                        return Err(Error::new(
                            ErrorKind::InvalidData,
                            format!(
                                "field {name} has more than one RuntimeInvisibleTypeAnnotations attribute"
                            ),
                        ));
                    } else {
                        runtime_invisible_type_annotations =
                            Some(annotations.iter().map(Into::into).collect());
                    }
                }
                MemberAttribute::Unknown(a) => unknown_attributes.push(a.clone()),
            }
        }

        Ok(Member {
            name,
            descriptor,
            visibility,
            signature,
            runtime_visible_annotations: runtime_visible_annotations.unwrap_or_default(),
            runtime_invisible_annotations: runtime_invisible_annotations.unwrap_or_default(),
            runtime_visible_type_annotations: runtime_visible_type_annotations.unwrap_or_default(),
            runtime_invisible_type_annotations: runtime_invisible_type_annotations
                .unwrap_or_default(),
            attributes: unknown_attributes.into(),
        })
    }

    fn make_annotation(raw: &raw::ClassFile, info: &annotation) -> std::io::Result<Annotation> {
        let descriptor = Self::get_field_descriptor(raw, info.type_index)?;
        let element_value_pairs = info
            .element_value_pairs
            .iter()
            .map(|evp| -> std::io::Result<_> {
                Ok(ElementValuePair {
                    name: raw.get_utf8(evp.element_name_index)?.clone(),
                    value: Self::make_element_value(raw, &evp.value)?,
                })
            })
            .collect::<Result<_, _>>()?;

        Ok(Annotation {
            descriptor,
            element_value_pairs,
        })
    }

    fn make_element_value(
        raw: &raw::ClassFile,
        info: &element_value,
    ) -> std::io::Result<ElementValue> {
        Ok(match info {
            element_value::Byte(index) => {
                ElementValue::Byte(raw.get_int(*index)?.try_into().map_err(|_| {
                    Error::new(
                        ErrorKind::InvalidData,
                        format!("expected byte at index {index}; int value too large"),
                    )
                })?)
            }
            element_value::Char(index) => {
                ElementValue::Char(raw.get_int(*index)?.try_into().map_err(|_| {
                    Error::new(
                        ErrorKind::InvalidData,
                        format!("expected u16 at index {index}; int value too large"),
                    )
                })?)
            }
            element_value::Double(index) => ElementValue::Double(raw.get_double(*index)?),
            element_value::Float(index) => ElementValue::Float(raw.get_float(*index)?),
            element_value::Int(index) => ElementValue::Int(raw.get_int(*index)?),
            element_value::Long(index) => ElementValue::Long(raw.get_long(*index)?),
            element_value::Short(index) => {
                ElementValue::Short(raw.get_int(*index)?.try_into().map_err(|_| {
                    Error::new(
                        ErrorKind::InvalidData,
                        format!("expected i16 at index {index}; int value too large"),
                    )
                })?)
            }
            element_value::Bool(index) => ElementValue::Boolean(raw.get_int(*index)? != 0),
            element_value::String(index) => ElementValue::String(raw.get_utf8(*index)?.clone()),
            element_value::Enum {
                type_name_index,
                const_name_index,
            } => ElementValue::Enum {
                descriptor: Self::get_field_descriptor(raw, *type_name_index)?,
                name: raw.get_utf8(*const_name_index)?.clone(),
            },
            element_value::Class(index) => {
                ElementValue::Class(Self::get_return_descriptor(raw, *index)?)
            }
            element_value::Annotation(info) => {
                ElementValue::Annotation(Self::make_annotation(raw, info)?)
            }
            element_value::Array(elements) => ElementValue::Array(
                elements
                    .iter()
                    .map(|e| Self::make_element_value(raw, e))
                    .collect::<Result<_, _>>()?,
            ),
        })
    }

    fn get_field_descriptor(
        raw: &raw::ClassFile,
        index: usize,
    ) -> std::io::Result<FieldDescriptor> {
        parse_field_descriptor(raw.get_utf8(index)?)
            .map_err(|_| {
                Error::new(
                    ErrorKind::InvalidData,
                    format!("Invalid descriptor at {index}"),
                )
            })
            .map(|(_, d)| d)
    }

    fn get_return_descriptor(
        raw: &raw::ClassFile,
        index: usize,
    ) -> std::io::Result<Option<FieldDescriptor>> {
        parse_return_descriptor(raw.get_utf8(index)?)
            .map_err(|_| {
                Error::new(
                    ErrorKind::InvalidData,
                    format!("Invalid descriptor at {index}"),
                )
            })
            .map(|(_, d)| d)
    }

    fn get_method_descriptor(
        raw: &raw::ClassFile,
        index: usize,
    ) -> std::io::Result<MethodDescriptor> {
        parse_method_descriptor(raw.get_utf8(index)?)
            .map_err(|_| {
                Error::new(
                    ErrorKind::InvalidData,
                    format!("Invalid descriptor at {index}"),
                )
            })
            .map(|(_, d)| d)
    }

    fn uses_preview_features(&self) -> bool {
        self.minor_version == u16::MAX
    }
}

#[derive(Debug)]
pub enum Nest {
    Host(Rc<str>),
    Members(Rc<[Rc<str>]>),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Provide {
    pub class: Rc<str>,
    pub provides_with: Rc<[Rc<str>]>,
}

#[derive(Debug)]
pub struct Export {
    pub name: Rc<str>,
    pub flags: ExportFlags,
    pub exports_to: Rc<[Rc<str>]>,
}

bitflags! {
    #[derive(Debug)]
    pub struct ExportFlags: u8 {
        const SYNTHETIC = 1 << 0;
        const MANDATED = 1 << 1;
    }
}

impl From<raw::attributes::ExportFlags> for ExportFlags {
    fn from(value: raw::attributes::ExportFlags) -> Self {
        let mut flags = ExportFlags::empty();
        if value.contains(raw::attributes::ExportFlags::ACC_SYNTHETIC) {
            flags |= ExportFlags::SYNTHETIC;
        }
        if value.contains(raw::attributes::ExportFlags::ACC_MANDATED) {
            flags |= ExportFlags::MANDATED;
        }

        flags
    }
}

#[derive(Debug)]
pub struct Require {
    pub name: Rc<str>,
    pub flags: RequireFlags,
    pub version: Option<Rc<str>>,
}

bitflags! {
    #[derive(Debug)]
    pub struct RequireFlags: u8 {
        const TRANSITIVE = 1 << 0;
        const STATIC_PHASE = 1 << 1;
        const SYNTHETIC = 1 << 2;
        const MANDATED = 1 << 3;
    }
}

impl From<raw::attributes::RequireFlags> for RequireFlags {
    fn from(value: raw::attributes::RequireFlags) -> Self {
        let mut flags = RequireFlags::empty();
        if value.contains(raw::attributes::RequireFlags::ACC_TRANSITIVE) {
            flags |= RequireFlags::TRANSITIVE;
        }
        if value.contains(raw::attributes::RequireFlags::ACC_STATIC_PHASE) {
            flags |= RequireFlags::STATIC_PHASE;
        }
        if value.contains(raw::attributes::RequireFlags::ACC_SYNTHETIC) {
            flags |= RequireFlags::SYNTHETIC;
        }
        if value.contains(raw::attributes::RequireFlags::ACC_MANDATED) {
            flags |= RequireFlags::MANDATED;
        }

        flags
    }
}

#[derive(Debug)]
pub struct BootstrapMethod {
    pub method: Ref<MethodDescriptor>,
    pub arguments: Rc<[Constant]>,
}

#[derive(Debug)]
pub struct Ref<Descriptor> {
    pub class: Rc<str>,
    pub name_and_type: NameAndType<Descriptor>,
}

#[derive(Debug)]
pub struct EnclosingMethod {
    pub class: Rc<str>,
    pub method: Option<NameAndType<MethodDescriptor>>,
}

#[derive(Debug)]
pub struct NameAndType<Descriptor> {
    pub name: Rc<str>,
    pub descriptor: Descriptor,
}

#[derive(Debug)]
pub struct InnerClass {
    pub inner_class: Rc<str>,
    pub visibility: Visibility,
    pub outer_class: Option<Rc<str>>,
    pub inner_name: Option<Rc<str>>,
    pub flags: InnerClassFlags,
}

bitflags! {
    #[derive(Debug)]
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

impl From<InnerClassAccessFlags> for InnerClassFlags {
    fn from(value: InnerClassAccessFlags) -> Self {
        let mut flags = InnerClassFlags::empty();
        if value.contains(InnerClassAccessFlags::ACC_STATIC) {
            flags |= InnerClassFlags::STATIC;
        }
        if value.contains(InnerClassAccessFlags::ACC_FINAL) {
            flags |= InnerClassFlags::FINAL;
        }
        if value.contains(InnerClassAccessFlags::ACC_INTERFACE) {
            flags |= InnerClassFlags::INTERFACE;
        }
        if value.contains(InnerClassAccessFlags::ACC_ABSTRACT) {
            flags |= InnerClassFlags::ABSTRACT;
        }
        if value.contains(InnerClassAccessFlags::ACC_SYNTHETIC) {
            flags |= InnerClassFlags::SYNTHETIC;
        }
        if value.contains(InnerClassAccessFlags::ACC_ANNOTATION) {
            flags |= InnerClassFlags::ANNOTATION;
        }
        if value.contains(InnerClassAccessFlags::ACC_ENUM) {
            flags |= InnerClassFlags::ENUM;
        }

        flags
    }
}

#[derive(Debug)]
pub enum Constant {
    Str(Rc<str>),
    Int(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    SecondHalf,
    Class(Rc<str>),
    String(Rc<str>),
    Fieldref(Ref<FieldDescriptor>),
    MethodRef(Ref<MethodDescriptor>),
    InterfaceMethodRef(Ref<MethodDescriptor>),
    // Might need to have fields...
    NameAndType,
    MethodHandle {
        kind: ReferenceKind,
        reference: Ref<MethodDescriptor>,
    },
    MethodType(MethodDescriptor),
    Dynamic(DynamicInfo),
    InvokeDynamic(DynamicInfo),
    Module(Rc<str>),
    Package(Rc<str>),
}

#[derive(Debug)]
pub struct DynamicInfo {
    pub bootstrap_method_index: usize,
    pub nat: NameAndType<MethodDescriptor>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Mutability {
    Final,
    Mutable,
    Volatile,
}

#[derive(Debug)]
pub struct TypeAnnotation<Target> {
    pub target: Target,
    pub target_path: Rc<[TypePathElement]>,
}

impl<T: Clone> From<&type_annotation<T>> for TypeAnnotation<T> {
    fn from(value: &type_annotation<T>) -> Self {
        Self {
            target: value.target.clone(),
            target_path: value.target_path.0.iter().map(Into::into).collect(),
        }
    }
}

#[derive(Debug)]
pub enum TypePathElement {
    Array,
    Nested,
    WildcardTypeArgumentBound,
    TypeArgument(usize),
}

impl From<&raw::attributes::TypePathElement> for TypePathElement {
    fn from(value: &raw::attributes::TypePathElement) -> Self {
        use raw::attributes::TypePathKind;

        match value.type_path_kind {
            TypePathKind::Array => Self::Array,
            TypePathKind::Nested => Self::Nested,
            TypePathKind::WildcardTypeArgumentBound => Self::WildcardTypeArgumentBound,
            TypePathKind::TypeArgument => Self::TypeArgument(value.type_argument_index as usize),
        }
    }
}

#[derive(Debug)]
pub struct Annotation {
    pub descriptor: FieldDescriptor,
    pub element_value_pairs: Rc<[ElementValuePair]>,
}

#[derive(Debug)]
pub struct ElementValuePair {
    pub name: Rc<str>,
    pub value: ElementValue,
}

#[derive(Debug)]
pub enum ElementValue {
    Byte(u8),
    Char(i16),
    Double(f64),
    Float(f32),
    Int(i32),
    Long(i64),
    Short(i16),
    Boolean(bool),
    String(Rc<str>),
    Enum {
        descriptor: FieldDescriptor,
        name: Rc<str>,
    },
    Class(Option<FieldDescriptor>),
    Annotation(Annotation),
    Array(Rc<[Self]>),
}

#[derive(Debug)]
pub enum Visibility {
    Private,
    Protected,
    Package,
    Public,
}

impl From<()> for Visibility {
    fn from(_: ()) -> Self {
        Self::Package
    }
}

impl From<MemberAccessFlags> for Visibility {
    fn from(value: raw::MemberAccessFlags) -> Self {
        if value.contains(raw::MemberAccessFlags::ACC_PRIVATE) {
            Self::Private
        } else if value.contains(raw::MemberAccessFlags::ACC_PROTECTED) {
            Self::Protected
        } else if value.contains(raw::MemberAccessFlags::ACC_PUBLIC) {
            Self::Public
        } else {
            Self::Package
        }
    }
}

impl From<MethodAccessFlags> for Visibility {
    fn from(value: MethodAccessFlags) -> Self {
        if value.contains(MethodAccessFlags::ACC_PRIVATE) {
            Self::Private
        } else if value.contains(MethodAccessFlags::ACC_PROTECTED) {
            Self::Protected
        } else if value.contains(MethodAccessFlags::ACC_PUBLIC) {
            Self::Public
        } else {
            Self::Package
        }
    }
}

impl From<InnerClassAccessFlags> for Visibility {
    fn from(value: InnerClassAccessFlags) -> Self {
        if value.contains(InnerClassAccessFlags::ACC_PRIVATE) {
            Self::Private
        } else if value.contains(InnerClassAccessFlags::ACC_PROTECTED) {
            Self::Protected
        } else if value.contains(InnerClassAccessFlags::ACC_PUBLIC) {
            Self::Public
        } else {
            Self::Package
        }
    }
}

#[derive(Debug)]
pub struct Method {
    pub member: Member<MethodDescriptor, MethodTypeAnnotationTarget>,
    pub code: Option<Code>,
    pub throws: Rc<[Rc<str>]>,
    pub runtime_visible_parameter_annotations: Rc<[Rc<[Annotation]>]>,
    pub runtime_invisible_parameter_annotations: Rc<[Rc<[Annotation]>]>,
    pub annotation_default: Option<ElementValue>,
    pub parameters: Rc<[Parameter]>,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: Rc<str>,
    pub flags: ParameterFlags,
}

bitflags! {
    #[derive(Debug)]
    pub struct ParameterFlags: u8 {
        const FINAL = 1 << 0;
        const SYNTHETIC = 1 << 1;
        const MANDATED = 1 << 2;
    }
}

impl From<ParameterAccessFlags> for ParameterFlags {
    fn from(value: ParameterAccessFlags) -> Self {
        let mut flags = ParameterFlags::empty();
        if value.contains(ParameterAccessFlags::ACC_FINAL) {
            flags |= ParameterFlags::FINAL;
        }
        if value.contains(ParameterAccessFlags::ACC_SYNTHETIC) {
            flags |= ParameterFlags::SYNTHETIC;
        }
        if value.contains(ParameterAccessFlags::ACC_MANDATED) {
            flags |= ParameterFlags::MANDATED;
        }

        flags
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct LocalVariableType {
    pub scope: Range<usize>,
    pub name: Rc<str>,
    pub descriptor: FieldDescriptor,
    pub variable_index: usize,
}

#[derive(Debug)]
pub struct LocalVariable {
    pub scope: Range<usize>,
    pub name: Rc<str>,
    pub descriptor: FieldDescriptor,
    pub variable_index: usize,
}

#[derive(Debug)]
pub struct StackMapFrame {
    pub offset_delta: u16,
    pub info: StackMapFrameInfo,
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct ExceptionHandler {
    pub active: Range<usize>,
    pub handler_pc: usize,
    pub catch_type: Option<Rc<str>>,
}

// TODO: make sure none of these conflict
bitflags! {
    #[derive(Clone, Copy, Debug)]
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
        const DEPRECATED = 1 << 9;
    }
}

impl From<MethodAccessFlags> for MethodFlags {
    fn from(value: MethodAccessFlags) -> Self {
        let mut flags = MethodFlags::empty();
        if value.contains(MethodAccessFlags::ACC_STATIC) {
            flags |= MethodFlags::STATIC;
        }
        if value.contains(MethodAccessFlags::ACC_FINAL) {
            flags |= MethodFlags::FINAL;
        }
        if value.contains(MethodAccessFlags::ACC_SYNCHRONIZED) {
            flags |= MethodFlags::SYNCHRONIZED;
        }
        if value.contains(MethodAccessFlags::ACC_BRIDGE) {
            flags |= MethodFlags::BRIDGE;
        }
        if value.contains(MethodAccessFlags::ACC_VARARGS) {
            flags |= MethodFlags::VARARGS;
        }
        if value.contains(MethodAccessFlags::ACC_NATIVE) {
            flags |= MethodFlags::NATIVE;
        }
        if value.contains(MethodAccessFlags::ACC_ABSTRACT) {
            flags |= MethodFlags::ABSTRACT;
        }
        if value.contains(MethodAccessFlags::ACC_STRICT) {
            flags |= MethodFlags::STRICT;
        }
        if value.contains(MethodAccessFlags::ACC_SYNTHETIC) {
            flags |= MethodFlags::SYNTHETIC;
        }

        flags
    }
}

#[derive(Debug)]
pub struct Field {
    pub member: Member<FieldDescriptor, FieldTypeAnnotationTarget>,
    pub flags: FieldFlags,
    pub constant_value: Option<Constant>,
    pub mutability: Mutability,
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
    #[derive(Clone, Copy, Debug)]
    pub struct FieldFlags: u8 {
        const STATIC = 1 << 0;
        const TRANSIENT = 1 << 1;
        const SYNTHETIC = 1 << 2;
        const ENUM = 1 << 3;
        const DEPRECATED = 1 << 4;
    }
}

impl From<MemberAccessFlags> for FieldFlags {
    fn from(value: raw::MemberAccessFlags) -> Self {
        let mut flags = FieldFlags::empty();
        if value.contains(raw::MemberAccessFlags::ACC_STATIC) {
            flags |= FieldFlags::STATIC;
        }
        if value.contains(raw::MemberAccessFlags::ACC_TRANSIENT) {
            flags |= FieldFlags::TRANSIENT;
        }
        if value.contains(raw::MemberAccessFlags::ACC_SYNTHETIC) {
            flags |= FieldFlags::SYNTHETIC;
        }
        if value.contains(raw::MemberAccessFlags::ACC_ENUM) {
            flags |= FieldFlags::ENUM;
        }

        flags
    }
}
