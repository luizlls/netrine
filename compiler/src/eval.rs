use std::ops::Neg;

use crate::mir;
use crate::types;
use crate::types::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct Value(u64);

impl Value {
    const NULL: Value = Value(0);

    fn number(number: f64) -> Value {
        Value(number.to_bits())
    }

    fn integer(integer: i64) -> Value {
        Value(integer as u64)
    }

    fn boolean(boolean: bool) -> Value {
        Value(boolean as u64)
    }

    fn as_number(self) -> f64 {
        f64::from_bits(self.0)
    }

    fn as_integer(self) -> i64 {
        self.0 as i64
    }

    fn as_boolean(self) -> bool {
        self.0 == 1
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Value {
        Value::integer(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Value {
        Value::number(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Value {
        Value::boolean(value)
    }
}

macro_rules! binary {
    ($instruction:expr, $loperand:expr, $op:tt, $roperand:expr) => {{
        if $instruction.type_ == types::NUMBER {
            ($loperand.as_number() $op $roperand.as_number()).into()
        } else {
            ($loperand.as_integer() $op $roperand.as_integer()).into()
        }
    }};
}

#[derive(Debug)]
struct VM {
    values: Vec<Value>,
}

impl VM {
    pub fn run(&mut self, module: &mir::Module) -> (Value, Type) {
        let mut ip = 0;
        while ip < module.instructions.len() {
            let instruction = &module.instructions[ip];
            ip += 1;
            match &instruction.kind {
                mir::InstructionKind::Integer(integer) => {
                    self.values[integer.target.index()] = Value::integer(integer.value);
                }
                mir::InstructionKind::Number(number) => {
                    self.values[number.target.index()] = Value::number(number.value);
                }
                mir::InstructionKind::Boolean(boolean) => {
                    self.values[boolean.target.index()] = Value::boolean(boolean.value);
                }
                mir::InstructionKind::ToNumber(to_number) => {
                    let value = self.values[to_number.source.index()];
                    let result = Value::number(value.as_integer() as f64);
                    self.values[to_number.target.index()] = result;
                }
                mir::InstructionKind::Unary(unary) => {
                    let operand = self.values[unary.operand.index()];
                    let result = match unary.operator {
                        mir::Operator::Not => Value::boolean(operand.as_integer() == 0),
                        mir::Operator::Pos => {
                            if instruction.type_ == types::NUMBER {
                                Value::number(operand.as_number().abs())
                            } else {
                                Value::integer(operand.as_integer().abs())
                            }
                        }
                        mir::Operator::Neg => {
                            if instruction.type_ == types::NUMBER {
                                Value::number(operand.as_number().neg())
                            } else {
                                Value::integer(operand.as_integer().neg())
                            }
                        }
                        _ => unreachable!(),
                    };
                    self.values[unary.target.index()] = result;
                }
                mir::InstructionKind::Binary(binary) => {
                    let lhs = self.values[binary.loperand.index()];
                    let rhs = self.values[binary.roperand.index()];
                    let result = match binary.operator {
                        mir::Operator::Add => binary!(instruction, lhs, +, rhs),
                        mir::Operator::Sub => binary!(instruction, lhs, -, rhs),
                        mir::Operator::Mul => binary!(instruction, lhs, *, rhs),
                        mir::Operator::Div => binary!(instruction, lhs, /, rhs),
                        mir::Operator::Mod => binary!(instruction, lhs, %, rhs),
                        mir::Operator::Eq => binary!(instruction, lhs, ==, rhs),
                        mir::Operator::Ne => binary!(instruction, lhs, !=, rhs),
                        mir::Operator::Lt => binary!(instruction, lhs, <, rhs),
                        mir::Operator::Le => binary!(instruction, lhs, <=, rhs),
                        mir::Operator::Gt => binary!(instruction, lhs, >, rhs),
                        mir::Operator::Ge => binary!(instruction, lhs, >=, rhs),
                        mir::Operator::Or => Value::boolean(lhs.as_boolean() || rhs.as_boolean()),
                        mir::Operator::And => Value::boolean(lhs.as_boolean() && rhs.as_boolean()),
                        mir::Operator::Pow => {
                            if instruction.type_ == types::NUMBER {
                                Value::number(lhs.as_number().powf(rhs.as_number()))
                            } else {
                                Value::integer(lhs.as_integer().pow(rhs.as_integer() as u32))
                            }
                        }
                        _ => unreachable!(),
                    };
                    self.values[binary.target.index()] = result;
                }
                mir::InstructionKind::Return(return_) => {
                    return if let Some(source) = return_.source {
                        let value = self.values[source.index()];
                        (value, instruction.type_)
                    } else {
                        (Value::NULL, types::NOTHING)
                    };
                }
            }
        }

        (Value::NULL, types::NOTHING)
    }
}

pub fn eval(module: &mir::Module) -> String {
    let mut vm = VM {
        values: vec![Value(0); 64],
    };

    let (result, type_) = vm.run(module);

    match type_ {
        types::INTEGER => format!("{}", result.as_integer()),
        types::NUMBER => format!("{}", result.as_number()),
        types::BOOLEAN => format!("{}", result.as_boolean()),
        _ => "null".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::eval;
    use crate::mir;
    use crate::types;

    #[test]
    fn testing() {
        let module = mir::Module {
            instructions: vec![
                mir::Instruction {
                    kind: mir::Number {
                        target: mir::Variable::new(0),
                        value: 5.2,
                    }
                    .into(),
                    type_: types::NUMBER,
                },
                mir::Instruction {
                    kind: mir::Integer {
                        target: mir::Variable::new(1),
                        value: 2,
                    }
                    .into(),
                    type_: types::INTEGER,
                },
                mir::Instruction {
                    kind: mir::ToNumber {
                        source: mir::Variable::new(1),
                        target: mir::Variable::new(2),
                    }
                    .into(),
                    type_: types::NUMBER,
                },
                mir::Instruction {
                    kind: mir::Unary {
                        operand: mir::Variable::new(0),
                        operator: mir::Operator::Neg,
                        target: mir::Variable::new(3),
                    }
                    .into(),
                    type_: types::NUMBER,
                },
                mir::Instruction {
                    kind: mir::Binary {
                        loperand: mir::Variable::new(2),
                        roperand: mir::Variable::new(3),
                        operator: mir::Operator::Mul,
                        target: mir::Variable::new(4),
                    }
                    .into(),
                    type_: types::NUMBER,
                },
                mir::Instruction {
                    kind: mir::Return {
                        source: Some(mir::Variable::new(4)),
                    }
                    .into(),
                    type_: types::NUMBER,
                },
            ],
        };

        assert_eq!(eval(&module), "-10.4");
    }
}
