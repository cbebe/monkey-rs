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
