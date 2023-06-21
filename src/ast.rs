use std::collections::HashMap;

#[allow(dead_code)]
pub enum Node<'a> {
    Program(Program<'a>),
    Statement(Statement<'a>),
    Expression(Expression<'a>),
}

#[derive(Debug)]
pub struct BlockStatement<'a>(pub Vec<Statement<'a>>);

impl<'a> std::fmt::Display for BlockStatement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(";\n")
        )
    }
}

#[derive(Debug)]
pub struct Program<'a>(pub BlockStatement<'a>);

impl<'a> std::fmt::Display for Program<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum Statement<'a> {
    Let(&'a str, Expression<'a>),
    Return(Expression<'a>),
    Expression(Expression<'a>),
    Block(BlockStatement<'a>),
}

impl<'a> std::fmt::Display for Statement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(ident, expr) => write!(f, "let {ident} = {expr}"),
            Statement::Return(expr) => write!(f, "return {expr}"),
            Statement::Expression(expr) => write!(f, "{expr}"),
            Statement::Block(body) => write!(f, "{body}"),
        }
    }
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

impl<'a> std::fmt::Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Unary::Neg => write!(f, "-"),
            Unary::Not => write!(f, "!"),
        }
    }
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

impl<'a> std::fmt::Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Binary::Add => write!(f, "+"),
            Binary::Sub => write!(f, "-"),
            Binary::Mul => write!(f, "*"),
            Binary::Div => write!(f, "/"),
            Binary::LT => write!(f, "<"),
            Binary::GT => write!(f, ">"),
            Binary::Eq => write!(f, "=="),
            Binary::Neq => write!(f, "!="),
        }
    }
}

#[derive(Debug)]
pub enum Literal<'a> {
    Identifier(&'a str),
    Boolean(bool),
    Integer(i64),
    String(&'a str),
    Array(Vec<Expression<'a>>),
    Hash(HashMap<Expression<'a>, Expression<'a>>),
    Function {
        args: Vec<&'a str>,
        body: BlockStatement<'a>,
    },
    If {
        condition: Box<Expression<'a>>,
        consequence: BlockStatement<'a>,
        alternative: Option<BlockStatement<'a>>,
    },
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
}

impl<'a> std::fmt::Display for Expression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(l) => match l {
                Literal::Identifier(v) => write!(f, "{v}"),
                Literal::Boolean(v) => write!(f, "{v}"),
                Literal::Integer(v) => write!(f, "{v}"),
                Literal::String(v) => write!(f, "{v}"),
                Literal::Array(v) => {
                    write!(
                        f,
                        "{}",
                        v.iter()
                            .map(|elem| elem.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }
                Literal::Hash(v) => {
                    write!(
                        f,
                        "{{\n{}\n}}",
                        v.iter()
                            .map(|(k, v)| format!("{k}: {v},"))
                            .collect::<Vec<String>>()
                            .join(",\n")
                    )
                }
                Literal::Function { args, body } => {
                    write!(f, "fn ({}) {{\n{body}\n}}", args.join(", "),)
                }
                Literal::If {
                    condition,
                    consequence,
                    alternative,
                } => {
                    write!(f, "if ({condition}) {{\n{consequence}\n}}",)?;
                    if let Some(block) = alternative {
                        write!(f, " else {{\n{block}\n}}")?;
                    }
                    Ok(())
                }
            },
            Expression::Prefix(oper, expr) => write!(f, "({oper}{expr})"),
            Expression::Index { left, index } => write!(f, "({left}[{index}])"),
            Expression::Infix(left, oper, right) => write!(f, "({left} {oper} {right})"),
            Expression::Call { function, args } => write!(
                f,
                "{function}({})",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}
