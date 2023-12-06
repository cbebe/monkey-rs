use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SymbolScope(&'static str);

const GLOBAL_SCOPE: SymbolScope = SymbolScope("GLOBAL");

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    name: String,
    scope: SymbolScope,
    pub index: u16,
}

#[derive(Clone)]
pub struct SymbolTable {
    store: BTreeMap<String, Symbol>,
    num_definitions: u16,
}

impl SymbolTable {
    pub const fn new() -> Self {
        Self {
            store: BTreeMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, arg: &str) -> Symbol {
        let symbol = Symbol {
            name: arg.to_owned(),
            index: self.num_definitions,
            scope: GLOBAL_SCOPE,
        };
        self.store.insert(arg.to_owned(), symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        self.store.get(name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::symbol_table::{Symbol, SymbolTable, GLOBAL_SCOPE};

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
}
