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
    Let(String, Expression<'a>),
    Return(Expression<'a>),
    Expression(Expression<'a>),
    Block(BlockStatement<'a>),
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LT,
    GT,
    Eq,
    NotEq,
}

#[derive(Debug)]
pub enum Expression<'a> {
    Identifier(&'a str),
    Boolean(bool),
    Integer(i64),
    String(String),
    Array(Vec<Expression<'a>>),
    Hash(HashMap<Expression<'a>, Expression<'a>>),
    Prefix(Operator, Box<Expression<'a>>),
    Index {
        left: Box<Expression<'a>>,
        index: Box<Expression<'a>>,
    },
    Infix {
        left: Box<Expression<'a>>,
        operator: Operator,
        right: Box<Expression<'a>>,
    },
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
