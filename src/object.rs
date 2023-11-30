use std::collections::BTreeMap;

use crate::util::str_vec;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Null,
    ReturnValue(Box<Object>),
    Array(Vec<Object>),
    Hash(BTreeMap<HashKey, HashPair>),
    Error(String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct HashKey {
    object_type: &'static str,
    value: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashPair {
    pub key: Object,
    pub value: Object,
}

const INITIAL_STATE: u64 = 0xcbf29ce484222325;
const PRIME: u64 = 0x100000001b3;

#[inline]
pub const fn fnv_hash(bytes: &[u8]) -> u64 {
    let mut hash = INITIAL_STATE;
    let mut i = 0;
    while i < bytes.len() {
        hash = hash ^ (bytes[i] as u64);
        hash = hash.wrapping_mul(PRIME);
        i += 1;
    }
    hash
}

pub trait Hashable {
    fn hash_key(&self) -> HashKey;
}

impl Hashable for i64 {
    fn hash_key(&self) -> HashKey {
        HashKey {
            object_type: "INT",
            value: *self as u64,
        }
    }
}

impl Hashable for bool {
    fn hash_key(&self) -> HashKey {
        HashKey {
            object_type: "BOOL",
            value: *self as u64,
        }
    }
}

impl Hashable for String {
    fn hash_key(&self) -> HashKey {
        HashKey {
            object_type: "STRING",
            value: fnv_hash(self.as_bytes()),
        }
    }
}

impl std::fmt::Display for HashPair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} => {}", self.key, self.value)
    }
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
            Self::Hash(x) => write!(
                f,
                "{{{}}}",
                x.iter()
                    .map(|(_, v)| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Error(x) => write!(f, "ERROR: {x}"),
        }
    }
}
