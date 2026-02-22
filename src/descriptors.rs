use nom::{
    IResult, Parser,
    branch::alt,
    bytes::take_until,
    character::char,
    combinator::map,
    multi::many0,
    sequence::{delimited, preceded},
};
use std::rc::Rc;

pub enum FieldDescriptor {
    Base(BaseType),
    Class(Rc<str>),
    Array(Box<Self>),
}

pub enum BaseType {
    Byte,
    Char,
    Double,
    Float,
    Int,
    Long,
    Short,
    Boolean,
}

macro_rules! chars_to_variants {
    ($($char:literal: $value:expr),* $(,)?) => {
        ($(map(char($char), |_| $value)),*)
    };
}

fn parse_base_type(input: &str) -> IResult<&str, BaseType> {
    alt(chars_to_variants! {
        'B': BaseType::Byte,
        'C': BaseType::Char,
        'D': BaseType::Double,
        'F': BaseType::Float,
        'I': BaseType::Int,
        'J': BaseType::Long,
        'S': BaseType::Short,
        'Z': BaseType::Boolean,
    })
    .parse(input)
}

fn parse_field_descriptor(input: &str) -> IResult<&str, FieldDescriptor> {
    alt((
        map(parse_base_type, FieldDescriptor::Base),
        map(
            delimited(char('L'), take_until(";"), char(';')),
            |s: &str| FieldDescriptor::Class(s.to_string().into()),
        ),
        map(preceded(char('['), parse_field_descriptor), |d| {
            FieldDescriptor::Array(Box::new(d))
        }),
    ))
    .parse(input)
}

pub struct MethodDescriptor {
    pub parameter_descriptors: Rc<[FieldDescriptor]>,
    pub return_descriptor: Option<FieldDescriptor>,
}

fn parse_method_descriptor(input: &str) -> IResult<&str, MethodDescriptor> {
    let (input, parameters) =
        delimited(char('('), many0(parse_field_descriptor), char(')')).parse(input)?;
    let (input, return_descriptor) =
        alt((map(parse_field_descriptor, Some), map(char('V'), |_| None))).parse(input)?;

    Ok((
        input,
        MethodDescriptor {
            parameter_descriptors: parameters.into(),
            return_descriptor,
        },
    ))
}
