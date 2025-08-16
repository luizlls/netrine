use std::collections::HashMap;

use crate::error::Result;
use crate::mir;
use crate::types::Type;

const WASM_MAGIC: [u8; 4] = [0x00, 0x61, 0x73, 0x6D];
const WASM_VERSION: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

// WASM section IDs
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

const SELECT: u8 = 0x1B;
const RETURN: u8 = 0x0F;
const END: u8 = 0x0B;

struct Wasm {
    output: Vec<u8>,
}

impl Wasm {
    fn new() -> Wasm {
        Wasm {
            output: Vec::new(),
        }
    }

    fn compile(mut self, module: &mir::Module) -> Vec<u8> {
        self.emit_header();
        self.emit_type_section();
        self.emit_function_section();
        self.emit_export_section();
        self.emit_code_section(module);

        self.output
    }

    fn emit_header(&mut self) {
        emit_all(&mut self.output, &WASM_MAGIC);
        emit_all(&mut self.output, &WASM_VERSION);
    }

    fn emit_type_section(&mut self) {
        let mut section = Vec::new();

        // one type definition
        emit_u32(&mut section, 1);
        // regular function
        emit_u8(&mut section, FUNCTION_TYPE);
         // no parameters
        emit_u32(&mut section, 0);
         // one return value
        emit_u32(&mut section, 1);
        emit_u8(&mut section, F64_TYPE);

        self.emit_section(&section, TYPE_SECTION);
    }

    fn emit_function_section(&mut self) {
        let mut section = Vec::new();

        // just one function
        emit_u32(&mut section, 1);
        // function 0 uses type 0
        emit_u32(&mut section, 0);

        self.emit_section(&section, FUNCTION_SECTION);
    }

    fn emit_export_section(&mut self) {
        let mut section = Vec::new();

        // export main function
        emit_u32(&mut section, 1);

        // exported function name
        let name = b"main";
        emit_u32(&mut section, name.len() as u32);
        emit_all(&mut section, name);

        emit_u8(&mut section, FUNCTION_EXPORT_TYPE);

        // main function index
        emit_u32(&mut section, 0);

        self.emit_section(&section, EXPORT_SECTION);
    }

    fn emit_code_section(&mut self, module: &mir::Module) {
        let mut section = Vec::new();

        // only one function
        emit_u32(&mut section, 1);

        let mut instructions = Vec::new();
        self.emit_locals(&module.instructions, &mut instructions);

        for (local_index, instruction) in module.instructions.iter().enumerate() {
            self.emit_instruction(instruction, local_index as u32, &mut instructions);
        }

        // load last instruction and return it
        emit_u8(&mut instructions, LOCAL_GET);
        emit_u32(&mut instructions, (module.instructions.len() - 1) as u32);
        emit_u8(&mut instructions, RETURN);

        emit_u8(&mut instructions, END);

        emit_u32(&mut section, instructions.len() as u32);
        emit_all(&mut section, &instructions);

        self.emit_section(&section, CODE_SECTION);
    }

    fn emit_locals(&self, instructions: &Vec<mir::Instruction>, output: &mut Vec<u8>) {
        // initially just declare one local_index for each instruction
        let mut locals: HashMap<u8, u32> = HashMap::new();

        for instruction in instructions {
            *locals.entry(self.instruction_type(instruction)).or_insert(0) += 1;
        }

        emit_u32(output, locals.len() as u32);

        for (wasm_type, count) in locals {
            emit_u32(output, count);
            emit_u8(output, wasm_type);
        }
    }

    fn instruction_type(&self, instruction: &mir::Instruction) -> u8 {
        match instruction.type_ {
            Type::Boolean => I32_TYPE,
            Type::Integer
          | Type::Number => F64_TYPE,
            _ => unreachable!()
        }
    }

    fn emit_instruction(&self, instruction: &mir::Instruction, local_index: u32, instructions: &mut Vec<u8>) {
        match &instruction.kind {
            mir::InstructionKind::Unary(unary) => self.emit_unary(unary, local_index, instructions),
            mir::InstructionKind::Binary(binary) => self.emit_binary(binary, local_index, instructions),
            mir::InstructionKind::Number(number) => self.emit_number(number, local_index, instructions),
            mir::InstructionKind::Integer(integer) => self.emit_integer(integer, local_index, instructions),
        }
    }

    fn emit_integer(&self, integer: &mir::Integer, local_index: u32, instructions: &mut Vec<u8>) {
        emit_u8(instructions, F64_CONST);
        // integers as numbers for now
        emit_f64(instructions, integer.value as f64);
        emit_u8(instructions, LOCAL_SET);
        emit_u32(instructions, local_index);
    }

    fn emit_number(&self, number: &mir::Number, local_index: u32, instructions: &mut Vec<u8>) {
        emit_u8(instructions, F64_CONST);
        emit_f64(instructions, number.value);
        emit_u8(instructions, LOCAL_SET);
        emit_u32(instructions, local_index);
    }

    fn emit_binary(&self, binary: &mir::Binary, local_index: u32, instructions: &mut Vec<u8>) {
        emit_u8(instructions, LOCAL_GET);
        emit_u32(instructions, binary.loperand.id());
        emit_u8(instructions, LOCAL_GET);
        emit_u32(instructions, binary.roperand.id());

        let operation = match binary.operator {
            mir::Operator::Add => F64_ADD,
            mir::Operator::Sub => F64_SUB,
            mir::Operator::Mul => F64_MUL,
            mir::Operator::Div => F64_DIV,
            mir::Operator::Eq  => F64_EQ,
            mir::Operator::Ne  => F64_NE,
            mir::Operator::Lt  => F64_LT,
            mir::Operator::Le  => F64_LE,
            mir::Operator::Gt  => F64_GT,
            mir::Operator::Ge  => F64_GE,
            mir::Operator::And => I32_AND,
            mir::Operator::Or  => I32_OR,
            _ => unreachable!("Binary instruction with unsupported operator {}", binary.operator)
        };
        emit_u8(instructions, operation);

        emit_u8(instructions, LOCAL_SET);
        emit_u32(instructions, local_index);
    }

    fn emit_unary(&self, unary: &mir::Unary, local_index: u32, instructions: &mut Vec<u8>) {
        emit_u8(instructions, LOCAL_GET);
        emit_u32(instructions, unary.operand.id());

        let operation = match unary.operator {
            mir::Operator::Pos => NOOP,
            mir::Operator::Neg => F64_NEG,
            mir::Operator::Not => {
                // compare the value with 0
                emit_u8(instructions, I32_CONST);
                emit_f64(instructions, 0.0);
                I32_EQ
            }
            _ => unreachable!()
        };
        emit_u8(instructions, operation);

        emit_u8(instructions, LOCAL_SET);
        emit_u32(instructions, local_index);
    }

    fn emit_section(&mut self, section: &[u8], section_id: u8) {
        emit_u8(&mut self.output, section_id);
        emit_u32(&mut self.output, section.len() as u32);
        emit_all(&mut self.output, section);
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

pub fn compile(module: &mir::Module) -> Result<Vec<u8>> {
    Ok(Wasm::new().compile(module))
}
