use crate::{
    ast::{self, BlockStatement, Expression},
    builtins,
    code::{self, Instructions},
    object::{CompiledFunction, Object},
    symbol_table::{
        SymbolScope, SymbolTable, BUILTIN_SCOPE, FREE_SCOPE, FUNCTION_SCOPE, GLOBAL_SCOPE,
        LOCAL_SCOPE,
    },
};

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
    UndefinedVariable(String),
    ArrayTooLong(usize),
    HashTooLong(usize),
    TooManyLocals(u16),
    TooManyArgs(usize),
    TooManyInstructions(usize),
    UnknownScope(SymbolScope),
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
    pub symbol_table: std::rc::Rc<std::cell::RefCell<SymbolTable>>,
}

pub struct Bytecode {
    pub instructions: code::Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        let symbol_table = SymbolTable::default().with_rc();
        {
            let mut s = symbol_table.borrow_mut();
            for i in 0..builtins::BUILTINS.len() {
                s.define_builtin(i.try_into().unwrap(), builtins::BUILTINS[i].0);
            }
        }
        Self {
            scopes: vec![CompilationScope::new()],
            constants: vec![],
            symbol_table,
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(CompilationScope::new());
        self.symbol_table = SymbolTable::new(Some((*self.symbol_table).clone().into())).with_rc();
    }

    pub fn leave_scope(&mut self) -> Instructions {
        let scope = self.scopes.pop().expect("has scope");
        let table: SymbolTable = (*self.symbol_table).borrow().clone();
        self.symbol_table = table.outer.unwrap();
        scope.instructions
    }

    pub fn with_state(self, symbol_table: SymbolTable, constants: Vec<Object>) -> Self {
        Self {
            scopes: self.scopes,
            constants,
            symbol_table: symbol_table.with_rc(),
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
                let symbol = self.symbol_table.borrow_mut().define(ident);
                self.compile(Node::Expression(expr))?;
                let opcode = match symbol.scope {
                    LOCAL_SCOPE => code::Opcode::SetLocal(
                        symbol
                            .index
                            .try_into()
                            .or(Err(Error::TooManyLocals(symbol.index)))?,
                    ),
                    GLOBAL_SCOPE => code::Opcode::SetGlobal(symbol.index),
                    e => return Err(Error::UnknownScope(e)),
                };
                self.emit(opcode);
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

    fn compile_function(
        &mut self,
        name: Option<&str>,
        params: &[&str],
        statements: BlockStatement,
    ) -> Result<(), Error> {
        self.enter_scope();
        if let Some(name) = name {
            self.symbol_table.borrow_mut().define_function_name(name);
        }
        for p in params {
            self.symbol_table.borrow_mut().define(p);
        }
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

        let num_definitions = self.symbol_table.borrow().num_definitions;
        let num_locals: u8 = num_definitions
            .try_into()
            .or(Err(Error::TooManyLocals(num_definitions)))?;

        let free_symbols = &self.symbol_table.borrow().free_symbols.clone();
        let instructions = self.leave_scope();
        for i in free_symbols {
            self.load_symbol(i)?;
        }

        let num_params: u8 = params
            .len()
            .try_into()
            .or(Err(Error::TooManyArgs(params.len())))?;
        let fn_obj = Object::Function(CompiledFunction {
            instructions,
            num_params,
            num_locals,
        });
        let idx = self.add_constant(fn_obj);
        self.emit(code::Opcode::Closure(
            idx,
            free_symbols.len().try_into().unwrap(),
        ));
        Ok(())
    }

    fn compile_if(
        &mut self,
        condition: Expression,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Result<(), Error> {
        self.compile(Node::Expression(condition))?;
        let jump_not_truthy_pos = self.emit(code::Opcode::JumpNotTruthy(9999));
        self.compile(Node::Block(consequence))?;

        self.remove_last_pop();

        let jump_pos = self.emit(code::Opcode::Jump(9999));

        let after_consequence_pos = self.current_scope_mut().instructions.len();
        let pos: u16 = after_consequence_pos
            .try_into()
            .or(Err(Error::TooManyInstructions(after_consequence_pos)))?;
        self.change_opcode(jump_not_truthy_pos, code::Opcode::JumpNotTruthy(pos));

        if let Some(alt) = alternative {
            self.compile(Node::Block(alt))?;
            self.remove_last_pop();
        } else {
            self.emit(code::Opcode::Null);
        }
        let after_alternative_pos = self.current_scope_mut().instructions.len();
        let pos: u16 = after_alternative_pos
            .try_into()
            .or(Err(Error::TooManyInstructions(after_alternative_pos)))?;
        self.change_opcode(jump_pos, code::Opcode::Jump(pos));
        Ok(())
    }

    fn load_symbol(&mut self, s: &crate::symbol_table::Symbol) -> Result<(), Error> {
        let idx = s.index;
        match s.scope {
            GLOBAL_SCOPE => self.emit(code::Opcode::GetGlobal(idx)),
            LOCAL_SCOPE => self.emit(code::Opcode::GetLocal(
                idx.try_into().or(Err(Error::TooManyLocals(idx)))?,
            )),
            BUILTIN_SCOPE => self.emit(code::Opcode::GetBuiltin(idx.try_into().unwrap())),
            FREE_SCOPE => self.emit(code::Opcode::GetFree(
                idx.try_into().or(Err(Error::TooManyLocals(idx)))?,
            )),
            FUNCTION_SCOPE => self.emit(code::Opcode::CurrentClosure),
            e => return Err(Error::UnknownScope(e)),
        };
        Ok(())
    }

    fn compile_literal(&mut self, lit: ast::Literal) -> Result<(), Error> {
        use ast::Literal;
        match lit {
            Literal::Function {
                name,
                args: params,
                body: statements,
            } => {
                self.compile_function(name, &params, statements)?;
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
                self.compile_if(*condition, consequence, alternative)?;
            }
            Literal::Identifier(x) => {
                let symbol = self
                    .symbol_table
                    .borrow_mut()
                    .resolve(x)
                    .ok_or_else(|| Error::UndefinedVariable(x.to_owned()))?;
                self.load_symbol(&symbol)?;
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
            Expression::Call(func, args) => {
                self.compile(Node::Expression(*func))?;
                let len = args.len();
                for a in args {
                    self.compile(Node::Expression(a))?;
                }
                self.emit(code::Opcode::Call(
                    len.try_into().or(Err(Error::TooManyArgs(len)))?,
                ));
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

#[cfg(test)]
mod tests {
    use super::Compiler;

    #[test]
    fn test_compiler_scopes() {
        use crate::code::Opcode::{Add, Mul, Sub};
        macro_rules! scope {
            ($name: ident) => {{
                $name.current_scope()
            }};
        }

        let mut compiler = Compiler::new();
        assert_eq!(compiler.scopes.len() - 1, 0);
        let global_table = compiler.symbol_table.clone();
        compiler.emit(Mul);
        compiler.enter_scope();
        assert_eq!(compiler.scopes.len() - 1, 1);
        compiler.emit(Sub);
        assert_eq!(scope!(compiler).instructions.len(), 1);
        let last = scope!(compiler).last_instruction.expect("last instruction");
        match last.opcode {
            Sub => (),
            _ => panic!("last_instruction.opcode wrong: {:?}", last.opcode),
        }
        assert_eq!(
            compiler.symbol_table.borrow().outer,
            Some(global_table.clone()),
            "compiler did not enclose global",
        );
        compiler.leave_scope();
        assert_eq!(compiler.scopes.len() - 1, 0);
        assert_eq!(
            compiler.symbol_table, global_table,
            "compiler did not restore global symbol table",
        );
        assert_eq!(
            compiler.symbol_table.borrow().outer,
            None,
            "compiler modified global symbol table incorrectly",
        );
        compiler.emit(Add);
        assert_eq!(scope!(compiler).instructions.len(), 2);
        let last = scope!(compiler).last_instruction.expect("last instruction");
        match last.opcode {
            Add => (),
            _ => panic!("last_instruction.opcode wrong: {:?}", last.opcode),
        }

        let previous = scope!(compiler)
            .previous_instruction
            .expect("previous instruction");
        match previous.opcode {
            Mul => (),
            _ => panic!("previous_instruction.opcode wrong: {:?}", last.opcode),
        }
    }
}
