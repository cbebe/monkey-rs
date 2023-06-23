use crate::util::str_vec;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Null,
    ReturnValue(Box<Object>),
    Array(Vec<Object>),
    Error(String),
}

struct HashPair {
    key: Object,
    value: Object,
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(x) => write!(f, "{x}"),
            Self::Boolean(x) => write!(f, "{x}"),
            Self::String(x) => write!(f, "\"{x}\""),
            Self::Null => write!(f, "null"),
            Self::ReturnValue(x) => write!(f, "{}", x.as_ref()),
            Self::Array(x) => write!(f, "[{}]", str_vec(x).join(", ")),
            Self::Error(x) => write!(f, "ERROR: {x}"),
        }
    }
}
