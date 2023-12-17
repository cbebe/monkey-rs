use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SymbolScope(&'static str);

pub const GLOBAL_SCOPE: SymbolScope = SymbolScope("GLOBAL");
pub const LOCAL_SCOPE: SymbolScope = SymbolScope("LOCAL");
pub const BUILTIN_SCOPE: SymbolScope = SymbolScope("BUILTIN");

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

    pub fn define_builtin(&mut self, index: u16, arg: &str) -> Symbol {
        let symbol = Symbol {
            name: arg.to_string(),
            index,
            scope: BUILTIN_SCOPE,
        };
        self.store.insert(arg.to_owned(), symbol.clone());
        symbol
    }

    pub fn define(&mut self, arg: &str) -> Symbol {
        let symbol = Symbol {
            name: arg.to_string(),
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

    use crate::symbol_table::{Symbol, SymbolTable, BUILTIN_SCOPE, GLOBAL_SCOPE, LOCAL_SCOPE};

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
        macro_rules! test_define {
            ($table: expr, $name: expr) => {{
                assert_eq!($table.borrow_mut().define($name), expected[$name]);
            }};
        }
        test_define!(global, "a");
        test_define!(global, "b");
        let first_local = SymbolTable::new(Some(global)).with_rc();
        test_define!(first_local, "c");
        test_define!(first_local, "d");
        let second_local = SymbolTable::new(Some(first_local)).with_rc();
        test_define!(second_local, "e");
        test_define!(second_local, "f");
    }

    macro_rules! test_symbols {
        ($expected: expr, $scope: expr) => {
            for sym in $expected {
                if let Some(ref result) = $scope.resolve(&sym.name) {
                    assert_eq!(result, sym);
                } else {
                    panic!("name {} not resolvable", sym.name)
                }
            }
        };
    }

    macro_rules! define_table {
        () => {{
            define_table!(SymbolTable::default())
        }};
        ($symbol_table: expr) => {{
            $symbol_table.with_rc()
        }};
        ($($symbol: tt),+) => {{
            define_table!(SymbolTable::default(), $($symbol),+)
        }};
        ($symbol_table: expr, $($symbol: tt),+) => {{
            let table = $symbol_table.with_rc();
            {
                #[allow(unused_mut)]
                let mut mut_table = table.borrow_mut();
                $(mut_table.define($symbol);)+
            }
            table
        }};
    }

    #[test]
    fn test_resolve_global() {
        let global = define_table!("a", "b");
        let expected = vec![
            Symbol::new("a", GLOBAL_SCOPE, 0),
            Symbol::new("b", GLOBAL_SCOPE, 1),
        ];
        test_symbols!(&expected, global.borrow());
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
        test_symbols!(&expected, local.borrow());
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
            test_symbols!(&expected, first_local.borrow());
        }
        let second_local = define_table!(SymbolTable::new(Some(first_local)), "e", "f");
        {
            let expected = vec![
                Symbol::new("a", GLOBAL_SCOPE, 0),
                Symbol::new("b", GLOBAL_SCOPE, 1),
                Symbol::new("e", LOCAL_SCOPE, 0),
                Symbol::new("f", LOCAL_SCOPE, 1),
            ];
            test_symbols!(&expected, second_local.borrow());
        }
    }

    #[test]
    fn test_define_resolve_builtins() {
        let global = define_table!();
        let expected = vec![
            Symbol::new("a", BUILTIN_SCOPE, 0),
            Symbol::new("c", BUILTIN_SCOPE, 1),
            Symbol::new("e", BUILTIN_SCOPE, 2),
            Symbol::new("f", BUILTIN_SCOPE, 3),
        ];
        {
            let mut g = global.borrow_mut();
            for i in 0..expected.len() {
                let idx: u16 = i.try_into().unwrap();
                g.define_builtin(idx, &expected[i].name);
            }
        }
        test_symbols!(&expected, global.borrow());
        let first_local = define_table!(SymbolTable::new(Some(global)));
        test_symbols!(&expected, first_local.borrow());
        let second_local = define_table!(SymbolTable::new(Some(first_local)));
        test_symbols!(&expected, second_local.borrow());
    }
}
