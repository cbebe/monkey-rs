use std::collections::HashMap;

#[allow(dead_code)]
pub enum Node<'a> {
    Program(Program<'a>),
    Statement(Statement<'a>),
    Expression(Expression<'a>),
}

#[derive(Debug)]
pub struct BlockStatement<'a>(pub Vec<Statement<'a>>);

#[derive(Debug)]
pub struct Program<'a>(pub Vec<Statement<'a>>);

#[derive(Debug)]
pub enum Statement<'a> {
    Let(&'a str, Expression<'a>),
    Return(Expression<'a>),
    Expression(Expression<'a>),
    Block(BlockStatement<'a>),
}

#[derive(Debug)]
pub enum Operator<'a> {
    Unary(Unary),
    Binary(Binary, Expression<'a>),
    Call(Vec<Expression<'a>>),
    Index(Expression<'a>),
}

#[derive(Debug)]
pub enum Unary {
    Neg,
    Not,
}

#[derive(Debug)]
pub enum Binary {
    Add,
    Sub,
    Mul,
    Div,
    LT,
    GT,
    Eq,
    Neq,
}

#[derive(Debug)]
pub enum Literal<'a> {
    Identifier(&'a str),
    Boolean(bool),
    Integer(i64),
    String(&'a str),
    Array(Vec<Expression<'a>>),
    Hash(HashMap<Expression<'a>, Expression<'a>>),
}

#[derive(Debug)]
pub enum Expression<'a> {
    Literal(Literal<'a>),
    Prefix(Unary, Box<Expression<'a>>),
    Index {
        left: Box<Expression<'a>>,
        index: Box<Expression<'a>>,
    },
    Infix(Box<Expression<'a>>, Binary, Box<Expression<'a>>),
    Call {
        function: Box<Expression<'a>>,
        args: Vec<Expression<'a>>,
    },
    If {
        condition: Box<Expression<'a>>,
        consequence: BlockStatement<'a>,
        alternative: Option<BlockStatement<'a>>,
    },
    Function {
        identifier: String,
        body: BlockStatement<'a>,
    },
}
