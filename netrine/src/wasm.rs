use crate::error::Result;
use crate::mir::{
    Binary, Instruction, InstructionId, InstructionKind, Integer, Module, Number, Operator, Unary,
};
use crate::types::{self, Type};

const WASM_MAGIC: [u8; 4] = [0x00, 0x61, 0x73, 0x6D];
const WASM_VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

const CUSTOM_SECTION: u8 = 0x00;
const TYPE_SECTION: u8 = 0x01;
const FUNCTION_SECTION: u8 = 0x03;
const TABLE_SECTION: u8 = 0x04;
const MEMORY_SECTION: u8 = 0x05;
const GLOBAL_SECTION: u8 = 0x06;
const EXPORT_SECTION: u8 = 0x07;
const CODE_SECTION: u8 = 0x0A;

const I32_TYPE: u8 = 0x7F;
const I64_TYPE: u8 = 0x7E;
const F32_TYPE: u8 = 0x7D;
const F64_TYPE: u8 = 0x7C;
const FUNCTION_TYPE: u8 = 0x60;
const EXTERNAL_TYPE: u8 = 0x65;
const FUNCTIONREF_TYPE: u8 = 0x70;

const FUNCTION_EXPORT_TYPE: u8 = 0x00;
const MEMORY_EXPORT_TYPE: u8 = 0x02;

const NOOP: u8 = 0x01;
const I32_CONST: u8 = 0x41;
const I64_CONST: u8 = 0x42;
const F32_CONST: u8 = 0x43;
const F64_CONST: u8 = 0x44;

const LOCAL_GET: u8 = 0x20;
const LOCAL_SET: u8 = 0x21;

const I32_ADD: u8 = 0x6A;
const I32_SUB: u8 = 0x6B;
const I32_MUL: u8 = 0x6C;
const I32_DIV_S: u8 = 0x6D;
const I32_REM_S: u8 = 0x6F;

const I64_ADD: u8 = 0x7C;
const I64_SUB: u8 = 0x7D;
const I64_MUL: u8 = 0x7E;
const I64_DIV_S: u8 = 0x7F;
const I64_REM_S: u8 = 0x81;

const F32_ADD: u8 = 0x92;
const F32_SUB: u8 = 0x93;
const F32_MUL: u8 = 0x94;
const F32_DIV: u8 = 0x95;

const F64_ADD: u8 = 0xA0;
const F64_SUB: u8 = 0xA1;
const F64_MUL: u8 = 0xA2;
const F64_DIV: u8 = 0xA3;

const I32_EQ: u8 = 0x46;
const I32_NE: u8 = 0x47;
const I32_AND: u8 = 0x71;
const I32_OR: u8 = 0x72;
const I32_LT_S: u8 = 0x48;
const I32_LE_S: u8 = 0x4C;
const I32_GT_S: u8 = 0x4A;
const I32_GE_S: u8 = 0x4E;

const I64_EQ: u8 = 0x51;
const I64_NE: u8 = 0x52;
const I64_LT_S: u8 = 0x53;
const I64_LE_S: u8 = 0x57;
const I64_GT_S: u8 = 0x55;
const I64_GE_S: u8 = 0x59;

const F32_EQ: u8 = 0x5B;
const F32_NE: u8 = 0x5C;
const F32_LT: u8 = 0x5D;
const F32_LE: u8 = 0x5F;
const F32_GT: u8 = 0x5E;
const F32_GE: u8 = 0x60;

const F64_EQ: u8 = 0x61;
const F64_NE: u8 = 0x62;
const F64_LT: u8 = 0x63;
const F64_LE: u8 = 0x65;
const F64_GT: u8 = 0x64;
const F64_GE: u8 = 0x66;
const F64_NEG: u8 = 0x9A;

const I32_EQZ: u8 = 0x45;
const I64_EQZ: u8 = 0x50;

const F64_CONVERT_S_I64: u8 = 0xB9;

const SELECT: u8 = 0x1B;
const RETURN: u8 = 0x0F;
const END: u8 = 0x0B;

struct Wasm<'w> {
    module: &'w Module,
    output: Vec<u8>,
}

impl<'w> Wasm<'w> {
    fn new(module: &'w Module) -> Wasm<'w> {
        Wasm {
            module,
            output: Vec::new(),
        }
    }

    fn compile(mut self, module: &Module) -> Vec<u8> {
        self.emit_header();
        self.emit_type_section();
        self.emit_function_section();
        self.emit_export_section();
        self.emit_code_section(module);

        self.output
    }

    fn emit_section(&mut self, section: &[u8], section_id: u8) {
        emit_u8(&mut self.output, section_id);
        emit_u32(&mut self.output, section.len() as u32);
        emit_all(&mut self.output, section);
    }

    fn emit_header(&mut self) {
        emit_all(&mut self.output, &WASM_MAGIC);
        emit_all(&mut self.output, &WASM_VERSION);
    }

    fn emit_type_section(&mut self) {
        let mut section = Vec::new();

        // one type definition (main function)
        emit_u32(&mut section, 1);
        // regular function
        emit_u8(&mut section, FUNCTION_TYPE);
        // no parameters
        emit_u32(&mut section, 0);
        // one return value
        emit_u32(&mut section, 1);
        emit_u8(&mut section, self.instruction_type(self.module.instructions.last().unwrap()));

        self.emit_section(&section, TYPE_SECTION);
    }

    fn emit_function_section(&mut self) {
        let mut section = Vec::new();

        // just one function (main)
        emit_u32(&mut section, 1);
        // function 0 uses type 0
        emit_u32(&mut section, 0);

        self.emit_section(&section, FUNCTION_SECTION);
    }

    fn emit_export_section(&mut self) {
        let mut section = Vec::new();

        // export only one function (main)
        emit_u32(&mut section, 1);

        let name = b"main";
        emit_u32(&mut section, name.len() as u32);
        emit_all(&mut section, name);

        emit_u8(&mut section, FUNCTION_EXPORT_TYPE);

        // main function index
        emit_u32(&mut section, 0);

        self.emit_section(&section, EXPORT_SECTION);
    }

    fn emit_code_section(&mut self, module: &Module) {
        let mut section = Vec::new();

        // code for only one function (main)
        emit_u32(&mut section, 1);

        let mut output = Vec::new();
        self.emit_locals(&mut output, &module.instructions);

        for (local_index, instruction) in module.instructions.iter().enumerate() {
            self.emit_instruction(&mut output, instruction, local_index as u32);
        }

        // TODO: Implement RETURN instruction in MIR
        emit_u8(&mut output, LOCAL_GET);
        emit_u32(&mut output, (module.instructions.len() - 1) as u32);
        emit_u8(&mut output, RETURN);

        emit_u8(&mut output, END);

        // one local per instruction
        emit_u32(&mut section, output.len() as u32);
        emit_all(&mut section, &output);

        self.emit_section(&section, CODE_SECTION);
    }

    fn emit_locals(&self, output: &mut Vec<u8>, instructions: &Vec<Instruction>) {
        emit_u32(output, instructions.len() as u32);

        for instruction in instructions {
            emit_u32(output, 1);
            emit_u8(output, self.instruction_type(instruction));
        }
    }

    fn instruction_type(&self, instruction: &Instruction) -> u8 {
        match instruction.type_ {
            types::NUMBER => F64_TYPE,
            types::INTEGER => I64_TYPE,
            types::BOOLEAN => I32_TYPE,
            _ => unreachable!(),
        }
    }

    fn emit_instruction(&self, output: &mut Vec<u8>, instruction: &Instruction, local_index: u32) {
        match &instruction.kind {
            InstructionKind::Number(number) => self.emit_number(output, number, local_index),
            InstructionKind::Integer(integer) => self.emit_integer(output, integer, local_index),
            InstructionKind::Unary(unary) => self.emit_unary(output, unary, local_index),
            InstructionKind::Binary(binary) => {
                self.emit_binary(output, binary, local_index, instruction.type_)
            }
        }
    }

    fn emit_number(&self, output: &mut Vec<u8>, number: &Number, local_index: u32) {
        emit_u8(output, F64_CONST);
        emit_f64(output, number.value);
        emit_u8(output, LOCAL_SET);
        emit_u32(output, local_index);
    }

    fn emit_integer(&self, output: &mut Vec<u8>, integer: &Integer, local_index: u32) {
        emit_u8(output, I64_CONST);
        emit_i64(output, integer.value);
        emit_u8(output, LOCAL_SET);
        emit_u32(output, local_index);
    }

    // TODO: Implement coercion in MIR
    fn coerced_type(
        &self,
        output: &mut Vec<u8>,
        result_type: Type,
        instruction_id: InstructionId,
    ) -> Type {
        let operand_type = self.module.get_type(&instruction_id);

        if operand_type == types::INTEGER && result_type == types::NUMBER {
            emit_u8(output, F64_CONVERT_S_I64);
            result_type
        } else {
            operand_type
        }
    }

    fn emit_binary(
        &self,
        output: &mut Vec<u8>,
        binary: &Binary,
        local_index: u32,
        result_type: Type,
    ) {
        emit_u8(output, LOCAL_GET);
        emit_u32(output, binary.loperand.id());

        let loperand_type = self.coerced_type(output, result_type, binary.loperand);

        emit_u8(output, LOCAL_GET);
        emit_u32(output, binary.roperand.id());

        let roperand_type = self.coerced_type(output, result_type, binary.roperand);

        let operation_type = match (loperand_type, roperand_type) {
            (types::BOOLEAN, types::BOOLEAN) => types::BOOLEAN,
            (types::INTEGER, types::INTEGER) => types::INTEGER,
            (types::NUMBER, _) | (_, types::NUMBER) => types::NUMBER,
            _ => unreachable!(),
        };

        let operation = match (operation_type, binary.operator) {
            (types::NUMBER, Operator::Add) => F64_ADD,
            (types::NUMBER, Operator::Sub) => F64_SUB,
            (types::NUMBER, Operator::Mul) => F64_MUL,
            (types::NUMBER, Operator::Div) => F64_DIV,
            (types::NUMBER, Operator::Eq) => F64_EQ,
            (types::NUMBER, Operator::Ne) => F64_NE,
            (types::NUMBER, Operator::Lt) => F64_LT,
            (types::NUMBER, Operator::Le) => F64_LE,
            (types::NUMBER, Operator::Gt) => F64_GT,
            (types::NUMBER, Operator::Ge) => F64_GE,

            (types::INTEGER, Operator::Add) => I64_ADD,
            (types::INTEGER, Operator::Sub) => I64_SUB,
            (types::INTEGER, Operator::Mul) => I64_MUL,
            (types::INTEGER, Operator::Eq) => I64_EQ,
            (types::INTEGER, Operator::Ne) => I64_NE,
            (types::INTEGER, Operator::Lt) => I64_LT_S,
            (types::INTEGER, Operator::Le) => I64_LE_S,
            (types::INTEGER, Operator::Gt) => I64_GT_S,
            (types::INTEGER, Operator::Ge) => I64_GE_S,

            (types::BOOLEAN, Operator::Eq) => I32_EQ,
            (types::BOOLEAN, Operator::Ne) => I32_NE,
            (types::BOOLEAN, Operator::And) => I32_AND,
            (types::BOOLEAN, Operator::Or) => I32_OR,

            (type_, operator) => unreachable!("unsupported operator {operator} for {type_} type"),
        };

        emit_u8(output, operation);

        emit_u8(output, LOCAL_SET);
        emit_u32(output, local_index);
    }

    fn emit_unary(&self, output: &mut Vec<u8>, unary: &Unary, local_index: u32) {
        emit_u8(output, LOCAL_GET);
        emit_u32(output, unary.operand.id());

        let operation = match unary.operator {
            Operator::Not => I32_EQZ,
            Operator::Pos => NOOP,
            Operator::Neg => match self.module.get_type(&unary.operand) {
                types::NUMBER => F64_NEG,
                types::INTEGER => {
                    emit_u8(output, I64_CONST);
                    emit_i64(output, -1);
                    I64_MUL
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        emit_u8(output, operation);

        emit_u8(output, LOCAL_SET);
        emit_u32(output, local_index);
    }
}

fn emit_u8(output: &mut Vec<u8>, value: u8) {
    output.push(value);
}

pub fn emit_u32(output: &mut Vec<u8>, value: u32) {
    emit_uleb128(output, value);
}

fn emit_i32(output: &mut Vec<u8>, value: i32) {
    emit_sleb128(output, value as i64);
}

fn emit_f64(output: &mut Vec<u8>, value: f64) {
    emit_all(output, &value.to_le_bytes());
}

fn emit_i64(output: &mut Vec<u8>, value: i64) {
    emit_sleb128(output, value);
}

pub fn emit_all(output: &mut Vec<u8>, value: &[u8]) {
    output.extend_from_slice(value);
}

fn emit_uleb128(output: &mut Vec<u8>, mut value: u32) {
    loop {
        let byte = (value & 0x7F) as u8;
        value >>= 7;
        if value == 0 {
            output.push(byte);
            break;
        } else {
            output.push(byte | 0x80);
        }
    }
}

fn emit_sleb128(output: &mut Vec<u8>, mut value: i64) {
    loop {
        let byte = (value & 0x7F) as u8;
        value >>= 7;
        if (value == 0 && (byte & 0x40) == 0) || (value == -1 && (byte & 0x40) != 0) {
            output.push(byte);
            break;
        } else {
            output.push(byte | 0x80);
        }
    }
}

pub fn compile(module: &Module) -> Result<Vec<u8>> {
    Ok(Wasm::new(module).compile(module))
}
