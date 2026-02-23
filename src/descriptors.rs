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

#[derive(Debug)]
pub enum FieldDescriptor {
    Base(BaseType),
    Class(Rc<str>),
    Array(Box<Self>),
}

#[derive(Debug)]
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

pub fn parse_field_descriptor(input: &str) -> IResult<&str, FieldDescriptor> {
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

#[derive(Debug)]
pub struct MethodDescriptor {
    pub parameter_descriptors: Rc<[FieldDescriptor]>,
    pub return_descriptor: Option<FieldDescriptor>,
}

pub fn parse_return_descriptor(input: &str) -> IResult<&str, Option<FieldDescriptor>> {
    alt((map(parse_field_descriptor, Some), map(char('V'), |_| None))).parse(input)
}

pub fn parse_method_descriptor(input: &str) -> IResult<&str, MethodDescriptor> {
    map(
        (
            delimited(char('('), many0(parse_field_descriptor), char(')')),
            parse_return_descriptor,
        ),
        |(parameters, return_descriptor)| MethodDescriptor {
            parameter_descriptors: parameters.into(),
            return_descriptor,
        },
    )
    .parse(input)
}
