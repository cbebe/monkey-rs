use crate::util::str_vec;

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockStatement<'a>(pub Vec<Statement<'a>>);

impl<'a> std::fmt::Display for BlockStatement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{};", str_vec(&self.0).join(";\n"))
    }
}

#[derive(Debug)]
pub struct Program<'a>(pub BlockStatement<'a>);

impl<'a> std::fmt::Display for Program<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Hash, PartialEq, PartialOrd, Ord, Eq)]
pub enum Statement<'a> {
    Let(&'a str, Expression<'a>),
    Return(Expression<'a>),
    Expression(Expression<'a>),
}

impl<'a> std::fmt::Display for Statement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let(ident, expr) => write!(f, "let {ident} = {expr}"),
            Self::Return(expr) => write!(f, "return {expr}"),
            Self::Expression(expr) => write!(f, "{expr}"),
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

impl<'a> Operator<'a> {
    pub fn fold(self, acc: Expression<'a>) -> Expression<'a> {
        match self {
            Self::Unary(u) => Expression::Prefix(u, Box::new(acc)),
            Self::Binary(b, expr) => Expression::Infix(Box::new(acc), b, Box::new(expr)),
            Self::Call(args) => Expression::Call(Box::new(acc), args),
            Self::Index(expr) => Expression::Index {
                left: Box::new(acc),
                index: Box::new(expr),
            },
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Unary {
    Neg,
    Not,
}

impl std::fmt::Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Neg => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
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

impl std::fmt::Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::LT => write!(f, "<"),
            Self::GT => write!(f, ">"),
            Self::Eq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Literal<'a> {
    Identifier(&'a str),
    Boolean(bool),
    Integer(i64),
    String(&'a str),
    Array(Vec<Expression<'a>>),
    Hash(std::collections::BTreeMap<Expression<'a>, Expression<'a>>),
    Function(Vec<&'a str>, BlockStatement<'a>),
    If(
        Box<Expression<'a>>,
        BlockStatement<'a>,
        Option<BlockStatement<'a>>,
    ),
}

impl<'a> std::fmt::Display for Literal<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(v) => write!(f, "\"{v}\""),
            Self::Identifier(v) => write!(f, "{v}"),
            Self::Boolean(v) => write!(f, "{v}"),
            Self::Integer(v) => write!(f, "{v}"),
            Self::Array(v) => {
                write!(f, "[{}]", str_vec(v).join(", "))
            }
            Self::Hash(v) => {
                write!(
                    f,
                    "{{\n{}\n}}",
                    v.iter()
                        .map(|(k, v)| format!("{k}: {v}"))
                        .collect::<Vec<String>>()
                        .join(",\n")
                )
            }
            Self::Function(args, body) => {
                write!(f, "fn ({}) {{\n{body}\n}}", args.join(", "),)
            }
            Self::If(condition, consequence, alternative) => {
                write!(f, "if ({condition}) {{\n{consequence}\n}}",)?;
                if let Some(block) = alternative {
                    write!(f, " else {{\n{block}\n}}")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expression<'a> {
    Literal(Literal<'a>),
    Prefix(Unary, Box<Expression<'a>>),
    Index {
        left: Box<Expression<'a>>,
        index: Box<Expression<'a>>,
    },
    Infix(Box<Expression<'a>>, Binary, Box<Expression<'a>>),
    Call(Box<Expression<'a>>, Vec<Expression<'a>>),
}

impl<'a> std::fmt::Display for Expression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => write!(f, "{l}"),
            Self::Prefix(oper, expr) => write!(f, "({oper}{expr})"),
            Self::Index { left, index } => write!(f, "({left}[{index}])"),
            Self::Infix(left, oper, right) => write!(f, "({left} {oper} {right})"),
            Self::Call(function, args) => write!(f, "{function}({})", str_vec(args).join(", ")),
        }
    }
}
