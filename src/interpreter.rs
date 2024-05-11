use std::fmt;
use crate::{
    Value, ValueTree,
};
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum ExecutionError {
    #[error("Stack is empty")]
    EmptyStack,
    #[error("Top of the stack is not a Bool")]
    ResultTypeError,
    #[error("The argument was expected to be of type X")]
    ArgumentTypeError,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operation {
    Bool(bool),
    Str(Box<str>),
    Int(i64),
    EnumValue(Box<str>),
    Member { name: Box<str>, pos: usize },
    Not,
    Greater,
    GreaterEq,
    Equal,
    LesserEq,
    Lesser,
    NotEqual,
    And,
    Or,
    In,
}

impl Operation {
    pub fn short_symbol(&self) -> char {
        match self {
            Self::LesserEq => '≤',
            Self::Lesser => '<',
            Self::Equal => '=',
            Self::NotEqual => '≠',
            Self::Greater => '>',
            Self::GreaterEq => '≥',
            Self::Not => '!',
            Self::And => '∧',
            Self::Or => '∨',
            Self::In => '∊',
            Self::Bool(false) => '0',
            Self::Bool(true) => '1',
            Self::Str(_) => '"',
            Self::Int(_) => '#',
            Self::EnumValue(_) => 'E',
            Self::Member { name, .. } => {
                let mut iter = name.chars().skip(1);
                match iter.next() {
                    None => '?',
                    Some(c) => c,
                }
            }
        }
    }
}
#[derive(PartialEq)]
pub struct Query {
    pub ops: Box<[Operation]>,
    pub false_jumps: Box<[usize]>,
    pub true_jumps: Box<[usize]>,
}

fn set_jump(ops: &[Operation], false_jumps: &mut Vec<usize>, index: usize, argn: usize) {
    match ops.get(index) {
        Some(Operation::Member { pos, .. }) => {
            false_jumps.insert(index, argn - pos);
        }
        Some(_other) => {}
        None => {} //ignore this
    }
}

impl Query {
    pub fn new(ops: Box<[Operation]>) -> Self {
        let mut false_jumps = vec![0; ops.len()];
        let true_jumps = vec![0; ops.len()];

        for (i, op) in ops.iter().enumerate() {
            match op {
                Operation::Greater
                | Operation::GreaterEq
                | Operation::Equal
                | Operation::LesserEq
                | Operation::Lesser
                | Operation::NotEqual => {
                    set_jump(&ops, &mut false_jumps, i - 1, 2);
                    set_jump(&ops, &mut false_jumps, i - 2, 2);
                }
                Operation::Not => {
                    set_jump(&ops, &mut false_jumps, i - 1, 1);
                }
                _ => {}
            }
        }

        for i in (0..false_jumps.len() - 1).rev() {
            let old = false_jumps[i];
            let next_jump = false_jumps[i + old];
            false_jumps[i] = old + next_jump;
        }
        Query {
            ops,
            false_jumps: false_jumps.into_boxed_slice(),
            true_jumps: true_jumps.into_boxed_slice(),
        }
    }
    pub fn iter(&self) -> QueryIter {
        QueryIter {
            query: self,
            next: 0,
        }
    }
}

impl fmt::Debug for Query {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ops: String = self.ops.iter().map(Operation::short_symbol).collect();
        let members: Vec<&Box<str>> = self
            .ops
            .iter()
            .filter_map(|op| {
                if let Operation::Member { name, .. } = op {
                    Some(name)
                } else {
                    None
                }
            })
            .collect();

        f.debug_struct("Query")
            .field("ops", &ops)
            .field("members", &members)
            .finish()
    }
}

pub struct QueryIter<'query> {
    query: &'query Query,
    next: usize,
}

impl<'query> Iterator for QueryIter<'query> {
    type Item = (&'query Operation, usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.next;

        let op = self.query.ops.get(current)?;
        let false_jump = self.query.false_jumps.get(current)?;
        let true_jump = self.query.true_jumps.get(current)?;

        self.next = current + 1;

        // Since there's no endpoint to a Fibonacci sequence, the `Iterator`
        // will never return `None`, and `Some` is always returned.
        Some((op, *false_jump, *true_jump))
    }
}
/* I need to reconsider this.
 * Maybe this should be just bytes without a type and the type is in the Operation?
 * For now this is clearly safer.
 */
#[derive(PartialEq, Debug)]
enum StackValue<'object> {
    Bool(bool),
    Str(&'object str),
    Int(i64),
    Object(&'object ValueTree),
    List(&'object[Value]),
}

impl<'object> From<&'object Value> for StackValue<'object> {
    fn from(value: &'object Value) -> StackValue<'object> {
        match value {
            Value::Bool(b) => StackValue::Bool(*b),
            Value::Str(v) => StackValue::Str(v),
            Value::Int(v) => StackValue::Int(*v),
            Value::List(values) => StackValue::List(values),
            Value::EnumValue(..) => todo!(),
            Value::Object(v) => StackValue::Object(v),
        }
    }
}

impl<'object> PartialEq<Value> for StackValue<'object> {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Self::Bool(a), Value::Bool(b)) =>{a==b},
            (Self::Int(a), Value::Int(b)) =>{a==b},
            (Self::Str(a), Value::Str(b)) =>{**a==**b},
            (Self::List(a), Value::List(b)) =>{a==b},
            (Self::Object(a), Value::Object(b)) =>{*a==b},
            _ => false
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
    while let Some((ops, false_jump, _true_jump)) = ops_iter.next() {
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
            Operation::EnumValue(_value) => {
                todo!("");
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
            Operation::In => {
                let b = stack.pop();
                let a = stack.pop();
                if let (Some(a), Some(b)) = (a, b) {
                    if let StackValue::List(b_list) = b {
                        let result = b_list.iter().any(|e| a == *e);
                        let result = StackValue::Bool(result);
                        stack.push(result);
                    } else {
                        return Err(ExecutionError::ArgumentTypeError);
                    }
                } else {
                    return Err(ExecutionError::EmptyStack);
                }
            }
        }
    }
    // Final Result shoould be at the end of the stack
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
    use crate::types::{Type, UnionType};
    use crate::types::{Class, ClassRef};
    use crate::ValueTree;

    fn obj() -> ClassRef {
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
        let code = parse_query("true").unwrap();
        assert_eq!(execute(&code, &ValueTree::new(obj())), Ok(Decision::Match));
    }
    #[test]
    fn test_eq() {
        let code = parse_query("true == true").unwrap();
        assert_eq!(execute(&code, &ValueTree::new(obj())), Ok(Decision::Match));
    }
    #[test]
    fn test_noteq() {
        let code = parse_query("false != true").unwrap();
        assert_eq!(execute(&code, &ValueTree::new(obj())), Ok(Decision::Match));
    }
    #[test]
    fn test_gteq() {
        let code = parse_query("60 >= 4").unwrap();
        assert_eq!(execute(&code, &ValueTree::new(obj())), Ok(Decision::Match));
    }
    #[test]
    fn test_gt() {
        let code = parse_query("60 > 4").unwrap();
        assert_eq!(execute(&code, &ValueTree::new(obj())), Ok(Decision::Match));
    }
    #[test]
    fn test_lt() {
        let code = parse_query("60 < 4").unwrap();
        assert_eq!(
            execute(&code, &ValueTree::new(obj())),
            Ok(Decision::Difference(Reason::Unequal))
        );
    }
    #[test]
    fn test_lteq() {
        let code = parse_query("60 <= 4").unwrap();
        assert_eq!(
            execute(&code, &ValueTree::new(obj())),
            Ok(Decision::Difference(Reason::Unequal))
        );
    }
    #[test]
    fn test_and() {
        let code = parse_query("60 <= 4 and true").unwrap();
        assert_eq!(
            execute(&code, &ValueTree::new(obj())),
            Ok(Decision::Difference(Reason::Unequal))
        );
    }
    #[test]
    fn test_or() {
        let code = parse_query("60 <= 4 and true or true").unwrap();
        assert_eq!(execute(&code, &ValueTree::new(obj())), Ok(Decision::Match));
    }
    #[test]
    fn test_member() {
        let mut b = Class::new("b");
        b.add_member("c", UnionType::from_vec(vec![Type::Bool])).unwrap();
        b.add_member("d", UnionType::from_vec(vec![Type::Int])).unwrap();
        let b = Rc::new(b);
        let mut a = Class::new("a");
        a.add_member("b", UnionType::from_vec(vec![Type::Class(b.clone())])).unwrap();
        let a = Rc::new(a);

        let mut new_class = Class::new("new_class");
        new_class.add_member("a", UnionType::from_vec(vec![Type::Class(a.clone())])).unwrap();
        new_class.add_member("test", UnionType::from_vec(vec![Type::Bool])).unwrap();
        let new_class = Rc::new(new_class);

        let mut b_value = ValueTree::new(b);
        b_value.insert("c", Value::Bool(true)).unwrap();
        b_value.insert("d", Value::Int(10)).unwrap();
        let mut a_value = ValueTree::new(a);
        a_value.insert("b", Value::Object(b_value)).unwrap();

        let mut vt = ValueTree::new(new_class);
        vt.insert("test", Value::Bool(true)).unwrap();
        vt.insert("a", Value::Object(a_value)).unwrap();

        let code = parse_query(".test").unwrap();
        assert_eq!(execute(&code, &vt), Ok(Decision::Match));
        let code = parse_query(".test and .a.b.c").unwrap();
        assert_eq!(execute(&code, &vt), Ok(Decision::Match));
        let code = parse_query(".test and .a.b.d == 10").unwrap();
        assert_eq!(execute(&code, &vt), Ok(Decision::Match));
        let code = parse_query(".test or .a.b.c ").unwrap();
        assert_eq!(execute(&code, &vt), Ok(Decision::Match));
    }
    #[test]
    fn test_in_op() {
        let mut new_class = Class::new("new_class");
        new_class.add_member("list", UnionType::from_vec(vec![Type::List(UnionType::from_vec(vec![Type::Int]))])).unwrap();
        new_class.add_member("list2", UnionType::from_vec(vec![Type::List(UnionType::from_vec(vec![Type::Int]))])).unwrap();
        let new_class = Rc::new(new_class);

        let mut vt = ValueTree::new(new_class);
        vt.insert("list", Value::List(vec![Value::int(4)])).unwrap();
        vt.insert("list2", Value::List(vec![Value::int(8), Value::int(4), Value::int(10), Value::int(7), Value::int(13), Value::int(40), Value::int(5)])).unwrap();

        let code = parse_query("4 in .list").unwrap();
        assert_eq!(execute(&code, &vt), Ok(Decision::Match));
        let code = parse_query("5 in .list2").unwrap();
        assert_eq!(execute(&code, &vt), Ok(Decision::Match));
    }
}
