use crate::{
    ast,
    code::{self, Instructions},
    object::Object,
};
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
    ArrayTooLong(usize),
    HashTooLong(usize),
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

#[derive(Clone)]
struct CompilationScope {
    instructions: code::Instructions,
    last_instruction: Option<EmmitedInstruction>,
    previous_instruction: Option<EmmitedInstruction>,
}

impl CompilationScope {
    pub const fn new() -> Self {
        Self {
            instructions: vec![],
            last_instruction: None,
            previous_instruction: None,
        }
    }
}

pub struct Compiler {
    scopes: Vec<CompilationScope>,
    constants: Vec<Object>,
    pub symbol_table: SymbolTable,
}

pub struct Bytecode {
    pub instructions: code::Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            scopes: vec![CompilationScope::new()],
            constants: vec![],
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(CompilationScope::new());
    }

    pub fn leave_scope(&mut self) -> Instructions {
        let scope = self.scopes.pop().expect("has scope");
        scope.instructions
    }

    pub fn with_state(self, symbol_table: SymbolTable, constants: Vec<Object>) -> Self {
        Self {
            scopes: self.scopes,
            constants,
            symbol_table,
        }
    }

    fn current_scope(&self) -> &CompilationScope {
        self.scopes.last().expect("has scope")
    }

    fn current_scope_mut(&mut self) -> &mut CompilationScope {
        self.scopes.last_mut().expect("has scope")
    }

    fn set_last_instruction(&mut self, op: code::Opcode, pos: usize) {
        let scope = self.current_scope_mut();
        let previous = scope.last_instruction;
        let last = EmmitedInstruction {
            opcode: op,
            position: pos,
        };
        scope.previous_instruction = previous;
        scope.last_instruction = Some(last);
    }

    fn add_instruction(&mut self, ins: &mut code::Instructions) -> usize {
        let scope = self.current_scope_mut();
        let pos = scope.instructions.len();
        scope.instructions.append(ins);
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
        self.current_scope_mut().instructions[pos..(new_instruction.len() + pos)]
            .copy_from_slice(new_instruction);
    }

    fn change_opcode(&mut self, pos: usize, op: code::Opcode) {
        self.replace_instruction(pos, &code::make(op));
    }

    fn remove_last_pop(&mut self) {
        let scope = self.current_scope_mut();
        if let Some(EmmitedInstruction {
            opcode: code::Opcode::Pop,
            position: pos,
        }) = scope.last_instruction
        {
            scope.instructions = scope.instructions[..pos].to_vec();
            scope.last_instruction = scope.previous_instruction;
        }
    }

    fn compile_statement(&mut self, stmt: ast::Statement) -> Result<(), Error> {
        use ast::Statement;
        match stmt {
            Statement::Let(ident, expr) => {
                self.compile(Node::Expression(expr))?;
                let symbol = self.symbol_table.define(ident);
                self.emit(code::Opcode::SetGlobal(symbol.index));
            }
            Statement::Expression(e) => {
                self.compile(Node::Expression(e))?;
                self.emit(code::Opcode::Pop);
            }
            Statement::Return(e) => {
                self.compile(Node::Expression(e))?;
                self.emit(code::Opcode::ReturnValue);
            }
        }
        Ok(())
    }

    fn compile_literal(&mut self, lit: ast::Literal) -> Result<(), Error> {
        use ast::Literal;
        match lit {
            Literal::Function(_params, statements) => {
                self.enter_scope();
                self.compile(Node::Block(statements))?;

                if let Some(EmmitedInstruction {
                    opcode: code::Opcode::Pop,
                    position: pos,
                }) = self.current_scope().last_instruction
                {
                    self.change_opcode(pos, code::Opcode::ReturnValue);
                    self.set_last_instruction(code::Opcode::ReturnValue, pos);
                }

                if let Some(EmmitedInstruction {
                    opcode: code::Opcode::ReturnValue,
                    position: _,
                }) = self.current_scope().last_instruction
                {
                } else {
                    self.emit(code::Opcode::Return);
                }

                let fn_obj = Object::Function(self.leave_scope());
                let idx = self.add_constant(fn_obj);
                self.emit(code::Opcode::Constant(idx));
            }
            Literal::Boolean(bool) => {
                self.emit(if bool {
                    code::Opcode::True
                } else {
                    code::Opcode::False
                });
            }
            Literal::Integer(int) => {
                let int_obj = Object::Integer(int);
                let idx = self.add_constant(int_obj);
                self.emit(code::Opcode::Constant(idx));
            }
            Literal::If(condition, consequence, alternative) => {
                self.compile(Node::Expression(*condition))?;
                let jump_not_truthy_pos = self.emit(code::Opcode::JumpNotTruthy(9999));
                self.compile(Node::Block(consequence))?;

                self.remove_last_pop();

                let jump_pos = self.emit(code::Opcode::Jump(9999));

                let after_consequence_pos = self.current_scope_mut().instructions.len();
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
                let after_alternative_pos = self.current_scope_mut().instructions.len();
                self.change_opcode(jump_pos, code::Opcode::Jump(after_alternative_pos as u16));
            }
            Literal::Identifier(x) => {
                let symbol = self
                    .symbol_table
                    .resolve(x)
                    .ok_or_else(|| Error::UndefinedVariable(x.to_owned()))?;
                self.emit(code::Opcode::GetGlobal(symbol.index));
            }
            Literal::String(x) => {
                let str_obj = Object::String(x.to_string());
                let idx = self.add_constant(str_obj);
                self.emit(code::Opcode::Constant(idx));
            }
            Literal::Array(arr) => {
                let len = arr.len();
                let size: Result<u16, _> = len.try_into();
                for el in arr {
                    self.compile(Node::Expression(el))?;
                }
                self.emit(code::Opcode::Array(size.or(Err(Error::ArrayTooLong(len)))?));
            }
            Literal::Hash(map) => {
                let len = map.len();
                let size: Result<u16, _> = (len * 2).try_into();
                for (k, v) in map {
                    self.compile(Node::Expression(k))?;
                    self.compile(Node::Expression(v))?;
                }
                self.emit(code::Opcode::Hash(size.or(Err(Error::HashTooLong(len)))?));
            }
        }
        Ok(())
    }

    fn compile_expr(&mut self, expr: ast::Expression) -> Result<(), Error> {
        use ast::Expression;
        match expr {
            Expression::Prefix(op, e) => {
                self.compile(Node::Expression(*e))?;
                match op {
                    ast::Unary::Neg => self.emit(code::Opcode::Minus),
                    ast::Unary::Not => self.emit(code::Opcode::Bang),
                };
            }
            Expression::Infix(left, op, right) => {
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
            Expression::Literal(l) => self.compile_literal(l)?,
            Expression::Index { left, index } => {
                self.compile(Node::Expression(*left))?;
                self.compile(Node::Expression(*index))?;
                self.emit(code::Opcode::Index);
            }
            Expression::Call(func, _args) => {
                self.compile(Node::Expression(*func))?;
                self.emit(code::Opcode::Call);
            }
        };
        Ok(())
    }

    fn compile(&mut self, node: Node) -> Result<(), Error> {
        match node {
            Node::Block(s) => {
                for statement in s.0 {
                    self.compile(Node::Statement(statement))?;
                }
            }
            Node::Statement(s) => self.compile_statement(s)?,
            Node::Expression(e) => self.compile_expr(e)?,
        };
        Ok(())
    }

    pub fn bytecode(self) -> Bytecode {
        Bytecode {
            instructions: self.current_scope().instructions.clone(),
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
    use std::{collections::BTreeMap, rc::Rc};

    use crate::{
        code::{self, Instructions, Opcode},
        util::test_utils::{self, compile_program, test_constants, test_instructions},
    };

    use super::{Compiler, Symbol, SymbolTable, GLOBAL_SCOPE};

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

    #[test]
    fn test_string_expressions() {
        use code::Opcode::{Add, Constant, Pop};
        use test_utils::Constant::String;
        run_compiler_tests(vec![
            (
                r#""monkey""#,
                vec![String(Rc::from("monkey"))],
                make(vec![Constant(0), Pop]),
            ),
            (
                r#""mon" + "key""#,
                vec![String(Rc::from("mon")), String(Rc::from("key"))],
                make(vec![Constant(0), Constant(1), Add, Pop]),
            ),
        ]);
    }

    #[test]
    fn test_array_literals() {
        use code::Opcode::{Add, Array, Constant, Mul, Pop, Sub};
        use test_utils::Constant::Int;
        run_compiler_tests(vec![
            ("[]", vec![], make(vec![Array(0), Pop])),
            (
                "[1, 2, 3]",
                vec![Int(1), Int(2), Int(3)],
                make(vec![Constant(0), Constant(1), Constant(2), Array(3), Pop]),
            ),
            (
                "[1 + 2, 3 - 4, 5 * 6]",
                vec![Int(1), Int(2), Int(3), Int(4), Int(5), Int(6)],
                make(vec![
                    Constant(0),
                    Constant(1),
                    Add,
                    Constant(2),
                    Constant(3),
                    Sub,
                    Constant(4),
                    Constant(5),
                    Mul,
                    Array(3),
                    Pop,
                ]),
            ),
        ]);
    }

    #[test]
    fn test_hash_literals() {
        use code::Opcode::{Add, Constant, Hash, Mul, Pop};
        use test_utils::Constant::Int;
        run_compiler_tests(vec![
            ("{}", vec![], make(vec![Hash(0), Pop])),
            (
                "{1: 2, 3: 4, 5: 6}",
                vec![Int(1), Int(2), Int(3), Int(4), Int(5), Int(6)],
                make(vec![
                    Constant(0),
                    Constant(1),
                    Constant(2),
                    Constant(3),
                    Constant(4),
                    Constant(5),
                    Hash(6),
                    Pop,
                ]),
            ),
            (
                "{1: 2 + 3, 4: 5 * 6}",
                vec![Int(1), Int(2), Int(3), Int(4), Int(5), Int(6)],
                make(vec![
                    Constant(0),
                    Constant(1),
                    Constant(2),
                    Add,
                    Constant(3),
                    Constant(4),
                    Constant(5),
                    Mul,
                    Hash(4),
                    Pop,
                ]),
            ),
        ]);
    }

    #[test]
    fn test_index_expresssions() {
        use code::Opcode::{Add, Array, Constant, Hash, Index, Pop, Sub};
        use test_utils::Constant::Int;
        run_compiler_tests(vec![
            (
                "[1, 2, 3][1 + 1]",
                vec![Int(1), Int(2), Int(3), Int(1), Int(1)],
                make(vec![
                    Constant(0),
                    Constant(1),
                    Constant(2),
                    Array(3),
                    Constant(3),
                    Constant(4),
                    Add,
                    Index,
                    Pop,
                ]),
            ),
            (
                "{1: 2}[2 - 1]",
                vec![Int(1), Int(2), Int(2), Int(1)],
                make(vec![
                    Constant(0),
                    Constant(1),
                    Hash(2),
                    Constant(2),
                    Constant(3),
                    Sub,
                    Index,
                    Pop,
                ]),
            ),
        ]);
    }

    #[test]
    fn test_functions() {
        use code::Opcode::{Add, Constant, Pop, ReturnValue};
        use test_utils::Constant::{Function, Int};
        run_compiler_tests(vec![
            (
                "fn() { return 5 + 10 }",
                vec![
                    Int(5),
                    Int(10),
                    Function(make(vec![Constant(0), Constant(1), Add, ReturnValue])),
                ],
                make(vec![Constant(2), Pop]),
            ),
            (
                "fn() { 5 + 10 }",
                vec![
                    Int(5),
                    Int(10),
                    Function(make(vec![Constant(0), Constant(1), Add, ReturnValue])),
                ],
                make(vec![Constant(2), Pop]),
            ),
            (
                "fn() { 1; 2 }",
                vec![
                    Int(1),
                    Int(2),
                    Function(make(vec![Constant(0), Pop, Constant(1), ReturnValue])),
                ],
                make(vec![Constant(2), Pop]),
            ),
        ]);
    }

    #[test]
    fn test_functions_without_return_value() {
        use code::Opcode::{Constant, Pop, Return};
        use test_utils::Constant::Function;
        run_compiler_tests(vec![(
            "fn() { }",
            vec![Function(make(vec![Return]))],
            make(vec![Constant(0), Pop]),
        )]);
    }

    #[test]
    fn test_compiler_scopes() {
        macro_rules! scope {
            ($name: ident) => {{
                $name.current_scope()
            }};
        }

        let mut compiler = Compiler::new();
        assert_eq!(compiler.scopes.len() - 1, 0);
        compiler.emit(code::Opcode::Mul);
        compiler.enter_scope();
        assert_eq!(compiler.scopes.len() - 1, 1);
        compiler.emit(code::Opcode::Sub);
        assert_eq!(scope!(compiler).instructions.len(), 1);
        let last = scope!(compiler).last_instruction.expect("last instruction");
        match last.opcode {
            code::Opcode::Sub => (),
            _ => panic!("last_instruction.opcode wrong: {:?}", last.opcode),
        }
        compiler.leave_scope();
        assert_eq!(compiler.scopes.len() - 1, 0);
        compiler.emit(code::Opcode::Add);
        assert_eq!(scope!(compiler).instructions.len(), 2);
        let last = scope!(compiler).last_instruction.expect("last instruction");
        match last.opcode {
            code::Opcode::Add => (),
            _ => panic!("last_instruction.opcode wrong: {:?}", last.opcode),
        }

        let previous = scope!(compiler)
            .previous_instruction
            .expect("previous instruction");
        match previous.opcode {
            code::Opcode::Mul => (),
            _ => panic!("previous_instruction.opcode wrong: {:?}", last.opcode),
        }
    }

    #[test]
    fn test_function_calls() {
        use code::Opcode::{Call, Constant, GetGlobal, Pop, ReturnValue, SetGlobal};
        use test_utils::Constant::{Function, Int};
        run_compiler_tests(vec![
            (
                "fn() { 24 }();",
                vec![Int(24), Function(make(vec![Constant(0), ReturnValue]))],
                make(vec![Constant(1), Call, Pop]),
            ),
            (
                "let noArg = fn() { 24 }; noArg();",
                vec![Int(24), Function(make(vec![Constant(0), ReturnValue]))],
                make(vec![Constant(1), SetGlobal(0), GetGlobal(0), Call, Pop]),
            ),
        ]);
    }

    fn run_compiler_tests(tests: Vec<Test>) {
        for (input, constants, instructions) in tests {
            let bytecode = compile_program(input);
            assert!(
                test_instructions(instructions.clone(), bytecode.instructions.clone()),
                "failed test for input {}.\nwant: {:?}\ngot: {:?}",
                &input,
                instructions,
                bytecode.instructions
            );
            if let Err(err) = test_constants(&constants, &bytecode.constants) {
                panic!("failed test for input {}.\n{err}", &input);
            }
        }
    }
}
