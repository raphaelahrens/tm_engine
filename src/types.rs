use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;

use std::rc::Rc;

use thiserror::Error;

use crate::{Str, Value};

#[derive(Debug, Clone, PartialEq)]
pub struct UnionType(Vec<Type>);

impl UnionType {
    pub fn new(datatypes:Vec<Type>) -> Self {
        UnionType(datatypes)
    }
    pub fn contains(&self, datatype:&Type) -> bool {
        self.0.contains(datatype)
    }
}

impl Display for UnionType {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let last = self.0.len()- 1;
        for t in &self.0[..last] {
            write!(f, "{} | ", t)?;
        }
        write!(f, "{}", self.0[last])
    }
}

impl From<&UnionType> for String {
    fn from(item: &UnionType) -> Self {
        format!("{}", item)
    }
}

#[derive(Debug, Clone)]
struct TypeMember {
    pub datatype: UnionType,
    pub default: Option<crate::Value>,
}

impl PartialEq for TypeMember {
    fn eq(&self, other: &Self) -> bool {
        self.datatype == other.datatype
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum TypeError {
    #[error("A type with the name already exists.({0})")]
    RedefinedType(String),
    #[error("A Unknown type was used. ({0})")]
    UndeclaredType(String),
    #[error("The given Type is an enum and a class type was expected. ({0})")]
    NotAClassType(String),
    #[error("The given Type is a class and an enum was expected. ({0})")]
    NotAnEnumType(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Int,
    Str,
    Object,
    Class(ClassRef),
    Enum(EnumRef),
    List(UnionType),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Bool => "Bool",
            Self::Int => "Int",
            Self::Str => "String",
            Self::Object => "Object",
            Self::Class(cls_ref) => {
                return write!(f, "{}", cls_ref);
            },
            Self::Enum(enum_ref) =>{
                return write!(f, "Enum {}", enum_ref);
            },
            Self::List(inner_type) => {
                return write!(f, "[{}]", inner_type);
            },
            //Self::Union(types) => {
            //    let last = dbg!(types.len())- 1;
            //    for t in &types[..last] {
            //        write!(f, "{} | ", t)?;
            //    }
            //    return write!(f, "{}", types[last]);
            //},
        };
        write!(f, "{}", s)
    }
}

impl From<&Type> for String {
    fn from(item: &Type) -> Self {
        format!("{}", item)
    }
}

#[derive(Debug)]
pub struct TypeTable {
    map: BTreeMap<Str, TypeDef>,
}

impl TypeTable {
    pub fn new() -> Self {
        TypeTable {
            map: BTreeMap::new(),
        }
    }
    fn get(&self, name: &str) -> Option<&TypeDef> {
        self.map.get(name)
    }
    pub fn add_class(&mut self, name: &str, datatype: Class) -> Result<(), TypeError> {
        self.add_type(name, TypeDef::Class(Rc::new(datatype)))
    }
    pub fn add_enum(&mut self, name: &str, datatype: Enum) -> Result<(), TypeError> {
        self.add_type(name, TypeDef::Enum(Rc::new(datatype)))
    }
    fn add_type(&mut self, name: &str, datatype: TypeDef) -> Result<(), TypeError> {
        if let Some(_) = self.map.insert(name.into(), datatype.clone()) {
            return Err(TypeError::RedefinedType(name.to_string()));
        }
        Ok(())
    }

    pub fn get_class(&self, name: &str) -> Result<&ClassRef, TypeError> {
        match self.get(name).ok_or(TypeError::UndeclaredType(name.to_string()))? {
            TypeDef::Class(class_ref) => Ok(class_ref),
            TypeDef::Enum(_) => Err(TypeError::NotAClassType(name.to_string())),
        }
    }
    pub fn get_enum(&self, name: &str) -> Result<&EnumRef, TypeError> {
        match self.get(name).ok_or(TypeError::UndeclaredType(name.to_string()))? {
            TypeDef::Class(_) => Err(TypeError::NotAnEnumType(name.to_string())),
            TypeDef::Enum(enum_ref)=> Ok(enum_ref),
        }
    }
    pub fn get_type(&self, name: &str) -> Result<Type, TypeError> {
        match self.get(name).ok_or(TypeError::UndeclaredType(name.to_string()))?{
            TypeDef::Class(class_ref) => {
                Ok(Type::Class(class_ref.clone()))
            },
            TypeDef::Enum(enum_ref) => {
                Ok(Type::Enum(enum_ref.clone()))
            },

        }
    }
}

pub type ClassRef = Rc<Class>;
pub type EnumRef = Rc<Enum>;

#[derive(Debug, Clone, PartialEq)]
enum TypeDef {
    Class(ClassRef),
    Enum(EnumRef),
}
#[derive(Debug, PartialEq)]
pub struct Enum {
    name: Str,
    pub variants: BTreeSet<Str>,
}
impl Enum {
    pub fn new(name: &str, variants:BTreeSet<Str>) -> Self {
        Self {
            name: name.into(),
            variants
        }
    }
}
impl Display for Enum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq)]
pub struct MemberInfo{
    name: Str,
    class_name: Str,
    datatype: String,
}

impl MemberInfo {
    fn new(class_name: &str, member_name: &str, datatype: &UnionType) -> Self {
        MemberInfo {
            name: member_name.into(),
            class_name: class_name.into(),
            datatype: datatype.into(),
        }
    }
    fn from_value(class_name: &str, member_name: &str, value: &Value) -> Self {
        MemberInfo {
            name: member_name.into(),
            class_name: class_name.into(),
            datatype: (&value.get_type()).into(),
        }
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum ClassError {
    #[error("A member \"{}\" was defined multiple times", .0.name)]
    RedefinedMember(MemberInfo),
    #[error("The value \"{}: {}\" in class \"{}\" was assigned a value with the wron Type ({})",
            .member.name, .member.datatype, .member.class_name, .datatype)]
    WrongMemberType{
        member: MemberInfo,
        datatype: String
    },
    #[error("Member \"{}\" was not defined for the class {}", .0.name, .0.class_name)]
    UnknownMember(MemberInfo),
}

#[derive(Debug, PartialEq)]
pub struct Class {
    name: Str,
    members: BTreeMap<Str, TypeMember>,
}

impl Class {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.into(),
            members: BTreeMap::new(),
        }
    }
    pub fn sub_class(name: &str, s: &Class) -> Self {
        Self {
            name: name.into(),
            members: s.members.clone(),
        }
    }
    fn add(&mut self, name: &str, member: TypeMember) -> Result<(), ClassError> {
        let datatype = member.datatype.clone();
        if let Some(_) = self.members.insert(name.into(), member) {
            return Err(ClassError::RedefinedMember(MemberInfo::new(&self.name, name, &datatype)));
        }
        Ok(())
    }

    pub fn add_member(&mut self, name: &str, datatype: UnionType) -> Result<(), ClassError> {
        self.add(
            name,
            TypeMember {
                datatype,
                default: None,
            },
        )
    }
    pub fn add_default_member(&mut self, name: &str, default: Value) -> Result<(), ClassError> {
        self.add(
            name,
            TypeMember {
                datatype: UnionType::new(vec![default.get_type()]),
                default: Some(default),
            },
        )
    }
    pub fn check_member_type(&self, name: &str, value: &crate::Value) -> Result<(), ClassError> {
        let def_member = self.members.get(name).ok_or(ClassError::UnknownMember(MemberInfo::from_value(&self.name, name, &value)))?;
        if !def_member.datatype.contains(&value.get_type()) {
            return Err(ClassError::WrongMemberType{
                                        member: MemberInfo::new(&self.name, name, &def_member.datatype),
                                        datatype: value.get_type().to_string(),
                                        });
        }
        Ok(())
    }
    pub fn default_value(&self) -> BTreeMap<Str, Value> {
        let mut tree = BTreeMap::new();
        for (name, member) in self.members.iter() {
            if let Some(value) = &member.default {
                tree.insert(name.clone(), value.clone());
            }
        }
        tree
    }
    pub fn members(&self) -> impl Iterator<Item = (&Box<str>, &UnionType)> {
        self.members.iter().map(|(n, m)| (n, &m.datatype))
    }
}
impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[cfg(test)]
mod test {
    
}
