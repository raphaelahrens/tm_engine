use crate::{
    parser::{Operation, Query},
    Value, ValueTree,
};
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum ExecutionError {
    #[error("Stack is empty")]
    EmptyStack,
    #[error("Top of the stack is not a Bool")]
    ResultTypeError,
}

#[derive(PartialEq, Debug)]
enum StackValue<'input> {
    Bool(bool),
    Str(&'input str),
    Int(i64),
}

impl<'object> From<&'object Value> for StackValue<'object> {
    fn from(value: &'object Value) -> StackValue<'object> {
        match value {
            Value::Bool(b) => StackValue::Bool(*b),
            Value::Str(v) => StackValue::Str(v),
            Value::Int(v) => StackValue::Int(*v),
            Value::Object(_v) => todo!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Reason {
    Unequal,
    TypeDifference,
    UnsetMember,
}

#[derive(Debug, PartialEq)]
pub enum Decision {
    Match,
    Difference(Reason),
}

impl From<Decision> for bool {
    fn from(decision: Decision) -> Self {
        match decision {
            Decision::Match => true,
            Decision::Difference(_r) => false,
        }
    }
}
impl From<&Decision> for bool {
    fn from(decision: &Decision) -> Self {
        match decision {
            Decision::Match => true,
            Decision::Difference(_r) => false,
        }
    }
}

pub fn execute(query: &Query, element: &ValueTree) -> Result<Decision, ExecutionError> {
    let mut stack = Vec::with_capacity(256);
    let mut ops_iter = query.iter();
    while let Some((ops, false_jump, true_jump)) = ops_iter.next() {
        match ops {
            Operation::Bool(value) => {
                stack.push(StackValue::Bool(*value));
            }
            Operation::Int(value) => {
                stack.push(StackValue::Int(*value));
            }
            Operation::Str(value) => {
                stack.push(StackValue::Str(value));
            }
            Operation::Member { name: path, pos: p } => match element.get(path) {
                Some(value) => {
                    stack.push(value.into());
                }
                None => {
                    for _i in 0..*p {
                        stack.pop();
                    }
                    stack.push(StackValue::Bool(false));
                    for _ in 0..false_jump {
                        ops_iter.next();
                    }
                }
            },
            Operation::Not => {
                if let Some(StackValue::Bool(value)) = stack.pop() {
                    stack.push(StackValue::Bool(!value));
                } else {
                    return Ok(Decision::Difference(Reason::TypeDifference));
                }
            }
            Operation::Equal => {
                let b = stack.pop();
                let a = stack.pop();
                if let (Some(a), Some(b)) = (a, b) {
                    let result = match (a, b) {
                        (StackValue::Bool(a), StackValue::Bool(b)) => a == b,
                        (StackValue::Int(a), StackValue::Int(b)) => a == b,
                        (StackValue::Str(a), StackValue::Str(b)) => a == b,
                        _ => {
                            return Ok(Decision::Difference(Reason::TypeDifference));
                        }
                    };
                    let result = StackValue::Bool(result);
                    stack.push(result);
                } else {
                    return Err(ExecutionError::EmptyStack);
                }
            }
            Operation::NotEqual => {
                let b = stack.pop();
                let a = stack.pop();
                if let (Some(a), Some(b)) = (a, b) {
                    let result = match (a, b) {
                        (StackValue::Bool(a), StackValue::Bool(b)) => a != b,
                        (StackValue::Int(a), StackValue::Int(b)) => a != b,
                        (StackValue::Str(a), StackValue::Str(b)) => a != b,
                        _ => {
                            return Ok(Decision::Difference(Reason::TypeDifference));
                        }
                    };
                    let result = StackValue::Bool(result);
                    stack.push(result);
                } else {
                    return Err(ExecutionError::EmptyStack);
                }
            }
            Operation::Greater => {
                let b = stack.pop();
                let a = stack.pop();
                if let (Some(a), Some(b)) = (a, b) {
                    let result = match (a, b) {
                        (StackValue::Bool(a), StackValue::Bool(b)) => a & !b,
                        (StackValue::Int(a), StackValue::Int(b)) => a > b,
                        (StackValue::Str(a), StackValue::Str(b)) => a > b,
                        _ => {
                            return Ok(Decision::Difference(Reason::TypeDifference));
                        }
                    };
                    let result = StackValue::Bool(result);
                    stack.push(result);
                } else {
                    return Err(ExecutionError::EmptyStack);
                }
            }
            Operation::GreaterEq => {
                let b = stack.pop();
                let a = stack.pop();
                if let (Some(a), Some(b)) = (a, b) {
                    let result = match (a, b) {
                        (StackValue::Bool(a), StackValue::Bool(b)) => a >= b,
                        (StackValue::Int(a), StackValue::Int(b)) => a >= b,
                        (StackValue::Str(a), StackValue::Str(b)) => a >= b,
                        _ => {
                            return Ok(Decision::Difference(Reason::TypeDifference));
                        }
                    };
                    let result = StackValue::Bool(result);
                    stack.push(result);
                } else {
                    return Err(ExecutionError::EmptyStack);
                }
            }
            Operation::LesserEq => {
                let b = stack.pop();
                let a = stack.pop();
                if let (Some(a), Some(b)) = (a, b) {
                    let result = match (a, b) {
                        (StackValue::Bool(a), StackValue::Bool(b)) => a <= b,
                        (StackValue::Int(a), StackValue::Int(b)) => a <= b,
                        (StackValue::Str(a), StackValue::Str(b)) => a <= b,
                        _ => {
                            return Ok(Decision::Difference(Reason::TypeDifference));
                        }
                    };
                    let result = StackValue::Bool(result);
                    stack.push(result);
                } else {
                    return Err(ExecutionError::EmptyStack);
                }
            }
            Operation::Lesser => {
                let b = stack.pop();
                let a = stack.pop();
                if let (Some(a), Some(b)) = (a, b) {
                    let result = match (a, b) {
                        (StackValue::Bool(a), StackValue::Bool(b)) => !a & b,
                        (StackValue::Int(a), StackValue::Int(b)) => a < b,
                        (StackValue::Str(a), StackValue::Str(b)) => a < b,
                        _ => {
                            return Ok(Decision::Difference(Reason::TypeDifference));
                        }
                    };
                    let result = StackValue::Bool(result);
                    stack.push(result);
                } else {
                    return Err(ExecutionError::EmptyStack);
                }
            }
            Operation::And => {
                let b = stack.pop();
                let a = stack.pop();
                if let (Some(a), Some(b)) = (a, b) {
                    let result = match (a, b) {
                        (StackValue::Bool(a), StackValue::Bool(b)) => a && b,
                        _ => {
                            return Ok(Decision::Difference(Reason::TypeDifference));
                        }
                    };
                    let result = StackValue::Bool(result);
                    stack.push(result);
                } else {
                    return Err(ExecutionError::EmptyStack);
                }
            }
            Operation::Or => {
                let b = stack.pop();
                let a = stack.pop();
                if let (Some(a), Some(b)) = (a, b) {
                    let result = match (a, b) {
                        (StackValue::Bool(a), StackValue::Bool(b)) => a || b,
                        _ => {
                            return Ok(Decision::Difference(Reason::TypeDifference));
                        }
                    };
                    let result = StackValue::Bool(result);
                    stack.push(result);
                } else {
                    return Err(ExecutionError::EmptyStack);
                }
            }
        }
    }
    match stack.pop() {
        Some(StackValue::Bool(true)) => Ok(Decision::Match),
        Some(StackValue::Bool(false)) => Ok(Decision::Difference(Reason::Unequal)),
        Some(_value) => Err(ExecutionError::ResultTypeError),
        None => Err(ExecutionError::EmptyStack),
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use super::*;
    use crate::parser::parse_query;
    use crate::ValueTree;
    use crate::types::Type;
    use crate::types::{Class, ClassRef};
    
    fn obj() -> ClassRef{
        Rc::new(Class::new("obj"))
    }

    #[test]
    fn test_single_value() {
        assert_eq!(
            execute(
                &Query::new(vec![Operation::Bool(true)].into()),
                &ValueTree::new(obj())
            ),
            Ok(Decision::Match)
        );
    }
    #[test]
    fn test_parser() {
        let (_rest, code) = parse_query("true").unwrap();
        assert_eq!(
            execute(&code, &ValueTree::new(obj())),
            Ok(Decision::Match)
        );
    }
    #[test]
    fn test_eq() {
        let (_rest, code) = parse_query("true == true").unwrap();
        assert_eq!(
            execute(&code, &ValueTree::new(obj())),
            Ok(Decision::Match)
        );
    }
    #[test]
    fn test_noteq() {
        let (_rest, code) = parse_query("false != true").unwrap();
        assert_eq!(
            execute(&code, &ValueTree::new(obj())),
            Ok(Decision::Match)
        );
    }
    #[test]
    fn test_gteq() {
        let (_rest, code) = parse_query("60 >= 4").unwrap();
        assert_eq!(
            execute(&code, &ValueTree::new(obj())),
            Ok(Decision::Match)
        );
    }
    #[test]
    fn test_gt() {
        let (_rest, code) = parse_query("60 > 4").unwrap();
        assert_eq!(
            execute(&code, &ValueTree::new(obj())),
            Ok(Decision::Match)
        );
    }
    #[test]
    fn test_lt() {
        let (_rest, code) = parse_query("60 < 4").unwrap();
        assert_eq!(
            execute(&code, &ValueTree::new(obj())),
            Ok(Decision::Difference(Reason::Unequal))
        );
    }
    #[test]
    fn test_lteq() {
        let (_rest, code) = parse_query("60 <= 4").unwrap();
        assert_eq!(
            execute(&code, &ValueTree::new(obj())),
            Ok(Decision::Difference(Reason::Unequal))
        );
    }
    #[test]
    fn test_and() {
        let (_rest, code) = parse_query("60 <= 4 and true").unwrap();
        assert_eq!(
            execute(&code, &ValueTree::new(obj())),
            Ok(Decision::Difference(Reason::Unequal))
        );
    }
    #[test]
    fn test_or() {
        let (_rest, code) = parse_query("60 <= 4 and true or true").unwrap();
        assert_eq!(
            execute(&code, &ValueTree::new(obj())),
            Ok(Decision::Match)
        );
    }
    #[test]
    fn test_member() {
        let mut b = Class::new("b");
        b.add_member("c", Type::Bool).unwrap();
        b.add_member("d", Type::Int).unwrap();
        let b = Rc::new(b);
        let mut a = Class::new("a");
        a.add_member("b", Type::Class(b.clone())).unwrap();
        let a = Rc::new(a);

        let mut new_class = Class::new("new_class");
        new_class.add_member("a", Type::Class(a.clone())).unwrap();
        new_class.add_member("test", Type::Bool).unwrap();
        let new_class = Rc::new(new_class);

        let mut b_value = ValueTree::new(b);
        b_value.insert("c", Value::Bool(true)).unwrap();
        b_value.insert("d", Value::Int(10)).unwrap();
        let mut a_value = ValueTree::new(a);
        a_value.insert("b", Value::Object(b_value)).unwrap();

        let mut vt = ValueTree::new(new_class);
        vt.insert("test", Value::Bool(true)).unwrap();
        vt.insert("a", Value::Object(a_value)).unwrap();

        let (_rest, code) = parse_query(".test").unwrap();
        assert_eq!(execute(&code, &vt), Ok(Decision::Match));
        let (_rest, code) = parse_query(".test and .a.b.c").unwrap();
        assert_eq!(execute(&code, &vt), Ok(Decision::Match));
        let (_rest, code) = parse_query(".test and .a.b.d == 10").unwrap();
        assert_eq!(execute(&code, &vt), Ok(Decision::Match));
        let (_rest, code) = parse_query(".test or .a.b.c ").unwrap();
        assert_eq!(execute(&code, &vt), Ok(Decision::Match));
    }
}
