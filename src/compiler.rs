use crate::{ast, code, object::Object};
use std::collections::BTreeMap;

#[derive(Debug)]
pub enum Node<'a> {
    Statement(ast::Statement<'a>),
    Block(ast::BlockStatement<'a>),
    Expression(ast::Expression<'a>),
}

impl<'a> std::fmt::Display for Node<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug)]
pub enum Error {
    NotYetImplemented(String),
    UndefinedVariable(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Clone, Copy)]
struct EmmitedInstruction {
    opcode: code::Opcode,
    position: usize,
}

pub struct Compiler {
    instructions: code::Instructions,
    constants: Vec<Object>,
    last_instruction: Option<EmmitedInstruction>,
    previous_instruction: Option<EmmitedInstruction>,
    pub symbol_table: SymbolTable,
}

pub struct Bytecode {
    pub instructions: code::Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub const fn new() -> Self {
        Self {
            instructions: vec![],
            constants: vec![],
            last_instruction: None,
            previous_instruction: None,
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn with_state(self, symbol_table: SymbolTable, constants: Vec<Object>) -> Self {
        Self {
            instructions: self.instructions,
            constants,
            last_instruction: self.last_instruction,
            previous_instruction: self.previous_instruction,
            symbol_table,
        }
    }

    fn set_last_instruction(&mut self, op: code::Opcode, pos: usize) {
        let previous = self.last_instruction;
        let last = EmmitedInstruction {
            opcode: op,
            position: pos,
        };
        self.previous_instruction = previous;
        self.last_instruction = Some(last);
    }

    fn add_instruction(&mut self, ins: &mut code::Instructions) -> usize {
        let pos = self.instructions.len();
        self.instructions.append(ins);
        pos
    }

    fn emit(&mut self, op: code::Opcode) -> usize {
        let mut ins = code::make(op);
        let pos = self.add_instruction(&mut ins);
        self.set_last_instruction(op, pos);
        pos
    }

    fn add_constant(&mut self, obj: Object) -> u16 {
        self.constants.push(obj);
        (self.constants.len() - 1)
            .try_into()
            .expect("number of constants exceeded")
    }

    pub fn compile_program(&mut self, program: ast::Program) -> Result<(), Error> {
        self.compile(Node::Block(program.0))
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: &[u8]) {
        self.instructions[pos..(new_instruction.len() + pos)].copy_from_slice(new_instruction);
    }

    fn change_opcode(&mut self, pos: usize, op: code::Opcode) {
        self.replace_instruction(pos, &code::make(op));
    }

    fn remove_last_pop(&mut self) {
        if let Some(EmmitedInstruction {
            opcode: code::Opcode::Pop,
            position: pos,
        }) = self.last_instruction
        {
            self.instructions = self.instructions[..pos].to_vec();
            self.last_instruction = self.previous_instruction;
        }
    }

    fn compile(&mut self, node: Node) -> Result<(), Error> {
        use ast::{Expression, Literal, Statement};
        match node {
            Node::Block(s) => {
                for statement in s.0 {
                    self.compile(Node::Statement(statement))?;
                }
            }
            Node::Statement(Statement::Let(ident, expr)) => {
                self.compile(Node::Expression(expr))?;
                let symbol = self.symbol_table.define(ident);
                self.emit(code::Opcode::SetGlobal(symbol.index));
            }
            Node::Statement(Statement::Expression(e)) => {
                self.compile(Node::Expression(e))?;
                self.emit(code::Opcode::Pop);
            }
            Node::Expression(Expression::Prefix(op, e)) => {
                self.compile(Node::Expression(*e))?;
                match op {
                    ast::Unary::Neg => self.emit(code::Opcode::Minus),
                    ast::Unary::Not => self.emit(code::Opcode::Bang),
                };
            }
            Node::Expression(Expression::Infix(left, op, right)) => {
                // Reverse operand emit so we can use the same instruction as GT
                if op == ast::Binary::LT {
                    self.compile(Node::Expression(*right))?;
                    self.compile(Node::Expression(*left))?;
                } else {
                    self.compile(Node::Expression(*left))?;
                    self.compile(Node::Expression(*right))?;
                }
                match op {
                    ast::Binary::Add => self.emit(code::Opcode::Add),
                    ast::Binary::Sub => self.emit(code::Opcode::Sub),
                    ast::Binary::Mul => self.emit(code::Opcode::Mul),
                    ast::Binary::Div => self.emit(code::Opcode::Div),
                    ast::Binary::LT | ast::Binary::GT => self.emit(code::Opcode::GreaterThan),
                    ast::Binary::Eq => self.emit(code::Opcode::Equal),
                    ast::Binary::Neq => self.emit(code::Opcode::NotEqual),
                };
            }
            Node::Expression(Expression::Literal(Literal::Boolean(bool))) => {
                self.emit(if bool {
                    code::Opcode::True
                } else {
                    code::Opcode::False
                });
            }
            Node::Expression(Expression::Literal(Literal::Integer(int))) => {
                let int_obj = Object::Integer(int);
                let idx = self.add_constant(int_obj);
                self.emit(code::Opcode::Constant(idx));
            }
            Node::Expression(Expression::Literal(Literal::If(
                condition,
                consequence,
                alternative,
            ))) => {
                self.compile(Node::Expression(*condition))?;
                let jump_not_truthy_pos = self.emit(code::Opcode::JumpNotTruthy(9999));
                self.compile(Node::Block(consequence))?;

                self.remove_last_pop();

                let jump_pos = self.emit(code::Opcode::Jump(9999));

                let after_consequence_pos = self.instructions.len();
                self.change_opcode(
                    jump_not_truthy_pos,
                    code::Opcode::JumpNotTruthy(after_consequence_pos as u16),
                );

                if let Some(alt) = alternative {
                    self.compile(Node::Block(alt))?;
                    self.remove_last_pop();
                } else {
                    self.emit(code::Opcode::Null);
                }
                let after_alternative_pos = self.instructions.len();
                self.change_opcode(jump_pos, code::Opcode::Jump(after_alternative_pos as u16));
            }
            Node::Expression(Expression::Literal(Literal::Identifier(x))) => {
                let symbol = self
                    .symbol_table
                    .resolve(x)
                    .ok_or_else(|| Error::UndefinedVariable(x.to_owned()))?;
                self.emit(code::Opcode::GetGlobal(symbol.index));
            }
            e => return Err(Error::NotYetImplemented(e.to_string())),
        }
        Ok(())
    }

    #[allow(clippy::missing_const_for_fn)]
    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SymbolScope(&'static str);

const GLOBAL_SCOPE: SymbolScope = SymbolScope("GLOBAL");

#[derive(Debug, Clone, PartialEq, Eq)]
struct Symbol {
    name: String,
    scope: SymbolScope,
    index: u16,
}

#[derive(Clone)]
pub struct SymbolTable {
    store: BTreeMap<String, Symbol>,
    num_definitions: u16,
}

impl SymbolTable {
    fn define(&mut self, arg: &str) -> Symbol {
        let symbol = Symbol {
            name: arg.to_owned(),
            index: self.num_definitions,
            scope: GLOBAL_SCOPE,
        };
        self.store.insert(arg.to_owned(), symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        self.store.get(name).cloned()
    }
}

impl SymbolTable {
    pub const fn new() -> Self {
        Self {
            store: BTreeMap::new(),
            num_definitions: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::{
        code::{self, Disassembled, Instructions, Opcode},
        util::test_utils::{self, compile_program, test_constants},
    };

    use super::{Symbol, SymbolTable, GLOBAL_SCOPE};

    type Test<'a> = (&'a str, Vec<test_utils::Constant>, Vec<Instructions>);

    #[test]
    fn test_define() {
        let expected = BTreeMap::from([
            (
                "a".to_owned(),
                Symbol {
                    name: "a".to_owned(),
                    scope: GLOBAL_SCOPE,
                    index: 0,
                },
            ),
            (
                "b".to_owned(),
                Symbol {
                    name: "b".to_owned(),
                    scope: GLOBAL_SCOPE,
                    index: 1,
                },
            ),
        ]);
        let mut global = SymbolTable::new();
        let a = global.define("a");
        assert_eq!(a, expected["a"]);
        let b = global.define("b");
        assert_eq!(b, expected["b"]);
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");
        let expected = vec![
            Symbol {
                name: "a".to_owned(),
                scope: GLOBAL_SCOPE,
                index: 0,
            },
            Symbol {
                name: "b".to_owned(),
                scope: GLOBAL_SCOPE,
                index: 1,
            },
        ];
        for sym in expected {
            if let Some(result) = global.resolve(&sym.name) {
                assert_eq!(result, sym);
            } else {
                panic!("name {} not resolvable", sym.name)
            }
        }
    }

    fn make(ops: Vec<Opcode>) -> Vec<Instructions> {
        ops.into_iter().map(code::make).collect()
    }

    #[test]
    fn test_integer_arithmetic() {
        use code::Opcode::{Add, Constant, Div, Minus, Mul, Pop, Sub};
        use test_utils::Constant::Int;
        run_compiler_tests(vec![
            (
                "1 + 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Constant(1), Add, Pop]),
            ),
            (
                "1; 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Pop, Constant(1), Pop]),
            ),
            (
                "1 - 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Constant(1), Sub, Pop]),
            ),
            (
                "1 * 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Constant(1), Mul, Pop]),
            ),
            (
                "2 / 1",
                vec![Int(2), Int(1)],
                make(vec![Constant(0), Constant(1), Div, Pop]),
            ),
            ("-1", vec![Int(1)], make(vec![Constant(0), Minus, Pop])),
        ]);
    }

    #[test]
    fn test_boolean_expressions() {
        use code::Opcode::{Bang, Constant, Equal, False, GreaterThan, NotEqual, Pop, True};
        use test_utils::Constant::Int;
        run_compiler_tests(vec![
            ("true", vec![], make(vec![True, Pop])),
            ("false", vec![], make(vec![False, Pop])),
            (
                "1 > 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Constant(1), GreaterThan, Pop]),
            ),
            (
                "1 < 2",
                vec![Int(2), Int(1)],
                make(vec![Constant(0), Constant(1), GreaterThan, Pop]),
            ),
            (
                "1 == 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Constant(1), Equal, Pop]),
            ),
            (
                "1 != 2",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), Constant(1), NotEqual, Pop]),
            ),
            ("true == false", vec![], make(vec![True, False, Equal, Pop])),
            (
                "true != false",
                vec![],
                make(vec![True, False, NotEqual, Pop]),
            ),
            ("!true", vec![], make(vec![True, Bang, Pop])),
        ]);
    }

    #[test]
    fn test_conditionals() {
        use code::Opcode::{Constant, Jump, JumpNotTruthy, Null, Pop, True};
        use test_utils::Constant::Int;
        run_compiler_tests(vec![
            (
                "if (true) { 10 }; 3333;",
                vec![Int(10), Int(3333)],
                make(vec![
                    // 0000
                    True,
                    // 0001
                    JumpNotTruthy(10),
                    // 0004
                    Constant(0),
                    // 0007
                    Jump(11),
                    // 0010
                    Null,
                    // 0011
                    Pop,
                    // 0012
                    Constant(1),
                    // 0015
                    Pop,
                ]),
            ),
            (
                "if (true) { 10 } else { 20 }; 3333;",
                vec![Int(10), Int(20), Int(3333)],
                make(vec![
                    // 0000
                    True,
                    // 0001
                    JumpNotTruthy(10),
                    // 0004
                    Constant(0),
                    // 0007
                    Jump(13),
                    // 0010
                    Constant(1),
                    // 0013
                    Pop,
                    // 0014
                    Constant(2),
                    // 0017
                    Pop,
                ]),
            ),
        ]);
    }

    #[test]
    fn test_global_let_statements() {
        use code::Opcode::{Constant, GetGlobal, Pop, SetGlobal};
        use test_utils::Constant::Int;
        run_compiler_tests(vec![
            (
                "let one = 1; let two = 2;",
                vec![Int(1), Int(2)],
                make(vec![Constant(0), SetGlobal(0), Constant(1), SetGlobal(1)]),
            ),
            (
                "let one = 1; one;",
                vec![Int(1)],
                make(vec![Constant(0), SetGlobal(0), GetGlobal(0), Pop]),
            ),
            (
                "let one = 1; let two = one; two",
                vec![Int(1)],
                make(vec![
                    Constant(0),
                    SetGlobal(0),
                    GetGlobal(0),
                    SetGlobal(1),
                    GetGlobal(1),
                    Pop,
                ]),
            ),
        ]);
    }

    fn run_compiler_tests(tests: Vec<Test>) {
        for (input, constants, instructions) in tests {
            let bytecode = compile_program(input);
            test_instructions(instructions, bytecode.instructions);
            if let Err(err) = test_constants(constants, bytecode.constants) {
                panic!("failed test for input {}.\n{err}", &input);
            }
        }
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let expected = expected.into_iter().flatten().collect::<Instructions>();
        assert_eq!(Disassembled(expected), Disassembled(actual));
    }
}
