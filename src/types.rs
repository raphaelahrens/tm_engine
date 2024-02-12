use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;
use std::path::Iter;
use std::rc::Rc;

use thiserror::Error;

use crate::{Str, Value, ValueTree};

#[derive(Debug, Clone)]
struct TypeMember {
    pub datatype: Type,
    pub default: Option<crate::Value>,
}

impl PartialEq for TypeMember {
    fn eq(&self, other: &Self) -> bool {
        self.datatype == other.datatype
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum TypeError {
    #[error("A type with the name already exists.")]
    RedefinedType,
    #[error("A Unknown type was used.")]
    UndeclaredType,
    #[error("The given Type is an enum expecte a class type")]
    NotAClassType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Int,
    Str,
    Enum,
    Object,
    Class(ClassRef),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Bool => "Bool",
            Self::Int => "Int",
            Self::Str => "String",
            Self::Object => "Object",
            Self::Enum => "Enum",
            Self::Class(cls_ref) => {
                return write!(f, "{}", cls_ref);
            }
        };
        write!(f, "{}", s)
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
            return Err(TypeError::RedefinedType);
        }
        Ok(())
    }

    pub fn get_class(&self, name: &str) -> Result<&ClassRef, TypeError> {
        match self.get(name).ok_or(TypeError::UndeclaredType)? {
            TypeDef::Class(class_ref) => Ok(class_ref),
            TypeDef::Enum(_) => Err(TypeError::NotAClassType),
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
    pub variants: BTreeSet<Str>,
}

#[derive(Error, Debug, PartialEq)]
pub enum ClassError {
    #[error("A member \"{member}\" was defined multiple times")]
    RedefinedMember { member: Str },
    #[error("The value \"{member}\" has the wrong Type for this member")]
    WrongMemberType { member: Str },
    #[error("Member was not defined for this class")]
    UnknownMember,
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
        if let Some(_) = self.members.insert(name.into(), member) {
            return Err(ClassError::RedefinedMember {
                member: name.into(),
            });
        }
        Ok(())
    }

    pub fn add_member(&mut self, name: &str, datatype: Type) -> Result<(), ClassError> {
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
                datatype: default.get_type(),
                default: Some(default),
            },
        )
    }
    pub fn check_member_type(&self, name: &str, value: &crate::Value) -> Result<(), ClassError> {
        let def_member = self.members.get(name).ok_or(ClassError::UnknownMember)?;
        if def_member.datatype != value.get_type() {
            return Err(ClassError::WrongMemberType {
                member: name.into(),
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
    pub fn members(&self) -> impl Iterator<Item = (&Box<str>, &Type)> {
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
    use super::*;
}
