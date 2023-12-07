use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SymbolScope(&'static str);

pub const GLOBAL_SCOPE: SymbolScope = SymbolScope("GLOBAL");
pub const LOCAL_SCOPE: SymbolScope = SymbolScope("LOCAL");

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    name: String,
    pub scope: SymbolScope,
    pub index: u16,
}

#[cfg(test)]
impl Symbol {
    pub fn new(name: &str, scope: SymbolScope, index: u16) -> Self {
        Self {
            name: name.to_string(),
            scope,
            index,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub outer: Option<std::rc::Rc<std::cell::RefCell<SymbolTable>>>,
    store: BTreeMap<String, Symbol>,
    pub num_definitions: u16,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new(None)
    }
}

impl SymbolTable {
    pub const fn new(outer: Option<std::rc::Rc<std::cell::RefCell<Self>>>) -> Self {
        Self {
            outer,
            store: BTreeMap::new(),
            num_definitions: 0,
        }
    }

    pub fn with_rc(self) -> std::rc::Rc<std::cell::RefCell<Self>> {
        std::rc::Rc::new(std::cell::RefCell::new(self))
    }

    pub fn define(&mut self, arg: &str) -> Symbol {
        let symbol = Symbol {
            name: arg.to_owned(),
            index: self.num_definitions,
            scope: match self.outer {
                Some(_) => LOCAL_SCOPE,
                None => GLOBAL_SCOPE,
            },
        };
        self.store.insert(arg.to_owned(), symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        self.store.get(name).map_or_else(
            || self.outer.as_ref().and_then(|o| o.borrow().resolve(name)),
            |s| Some(s.clone()),
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::symbol_table::{Symbol, SymbolTable, GLOBAL_SCOPE, LOCAL_SCOPE};

    #[test]
    fn test_define() {
        let expected = BTreeMap::from(
            [
                Symbol::new("a", GLOBAL_SCOPE, 0),
                Symbol::new("b", GLOBAL_SCOPE, 1),
                Symbol::new("c", LOCAL_SCOPE, 0),
                Symbol::new("d", LOCAL_SCOPE, 1),
                Symbol::new("e", LOCAL_SCOPE, 0),
                Symbol::new("f", LOCAL_SCOPE, 1),
            ]
            .map(|s| (s.name.clone(), s)),
        );
        let global = SymbolTable::default().with_rc();
        {
            let a = global.borrow_mut().define("a");
            assert_eq!(a, expected["a"]);
        }
        {
            let b = global.borrow_mut().define("b");
            assert_eq!(b, expected["b"]);
        }
        let first_local = SymbolTable::new(Some(global)).with_rc();
        {
            let c = first_local.borrow_mut().define("c");
            assert_eq!(c, expected["c"]);
        }
        {
            let d = first_local.borrow_mut().define("d");
            assert_eq!(d, expected["d"]);
        }
        let second_local = SymbolTable::new(Some(first_local)).with_rc();
        {
            let e = second_local.borrow_mut().define("e");
            assert_eq!(e, expected["e"]);
        }
        {
            let f = second_local.borrow_mut().define("f");
            assert_eq!(f, expected["f"]);
        }
    }

    macro_rules! test_symbols {
        ($expected: expr, $scope: expr) => {
            for sym in $expected {
                if let Some(result) = $scope.resolve(&sym.name) {
                    assert_eq!(result, sym);
                } else {
                    panic!("name {} not resolvable", sym.name)
                }
            }
        };
    }

    macro_rules! define_table {
        ($($symbol: tt),+) => {{
            define_table!(SymbolTable::default(), $($symbol),+)
        }};
        ($symbol_table: expr, $($symbol: tt),+) => {{
            let table = $symbol_table.with_rc();
            {
                let mut mut_table = table.borrow_mut();
                $(mut_table.define($symbol);)+
            }
            table
        }}
    }

    #[test]
    fn test_resolve_global() {
        let global = define_table!("a", "b");
        let expected = vec![
            Symbol::new("a", GLOBAL_SCOPE, 0),
            Symbol::new("b", GLOBAL_SCOPE, 1),
        ];
        test_symbols!(expected, global.borrow());
    }

    #[test]
    fn test_resolve_local() {
        let global = define_table!("a", "b");
        let local = define_table!(SymbolTable::new(Some(global)), "c", "d");
        let expected = vec![
            Symbol::new("a", GLOBAL_SCOPE, 0),
            Symbol::new("b", GLOBAL_SCOPE, 1),
            Symbol::new("c", LOCAL_SCOPE, 0),
            Symbol::new("d", LOCAL_SCOPE, 1),
        ];
        test_symbols!(expected, local.borrow());
    }

    #[test]
    fn test_resolve_nested_local() {
        let global = define_table!("a", "b");
        let first_local = define_table!(SymbolTable::new(Some(global)), "c", "d");
        {
            let expected = vec![
                Symbol::new("a", GLOBAL_SCOPE, 0),
                Symbol::new("b", GLOBAL_SCOPE, 1),
                Symbol::new("c", LOCAL_SCOPE, 0),
                Symbol::new("d", LOCAL_SCOPE, 1),
            ];
            test_symbols!(expected, first_local.borrow());
        }
        let second_local = define_table!(SymbolTable::new(Some(first_local)), "e", "f");
        {
            let expected = vec![
                Symbol::new("a", GLOBAL_SCOPE, 0),
                Symbol::new("b", GLOBAL_SCOPE, 1),
                Symbol::new("e", LOCAL_SCOPE, 0),
                Symbol::new("f", LOCAL_SCOPE, 1),
            ];
            test_symbols!(expected, second_local.borrow());
        }
    }
}
