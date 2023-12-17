use crate::compiler_tests::{func::make, run_compiler_tests};
use std::rc::Rc;

use crate::{
    code::{
        self, Instructions,
        Opcode::{
            Add, Array, Call, Closure, Constant, CurrentClosure, GetBuiltin, GetFree, GetGlobal,
            GetLocal, Pop, Return, ReturnValue, SetGlobal, SetLocal, Sub,
        },
    },
    util::test_utils::Constant::{Function, Int, String},
};

#[test]
fn test_functions() {
    run_compiler_tests(vec![
        (
            "fn() { return 5 + 10 }",
            vec![
                Int(5),
                Int(10),
                Function(make![Constant(0), Constant(1), Add, ReturnValue]),
            ],
            make![Closure(2, 0), Pop],
        ),
        (
            "fn() { 5 + 10 }",
            vec![
                Int(5),
                Int(10),
                Function(make![Constant(0), Constant(1), Add, ReturnValue]),
            ],
            make![Closure(2, 0), Pop],
        ),
        (
            "fn() { 1; 2 }",
            vec![
                Int(1),
                Int(2),
                Function(make![Constant(0), Pop, Constant(1), ReturnValue]),
            ],
            make![Closure(2, 0), Pop],
        ),
    ]);
}

#[test]
fn test_functions_without_return_value() {
    run_compiler_tests(vec![(
        "fn() { }",
        vec![Function(make![Return])],
        make![Closure(0, 0), Pop],
    )]);
}

#[test]
fn test_function_calls() {
    run_compiler_tests(vec![
        (
            "fn() { 24 }();",
            vec![Int(24), Function(make![Constant(0), ReturnValue])],
            make![Closure(1, 0), Call(0), Pop],
        ),
        (
            "let noArg = fn() { 24 }; noArg();",
            vec![Int(24), Function(make![Constant(0), ReturnValue])],
            make![Closure(1, 0), SetGlobal(0), GetGlobal(0), Call(0), Pop,],
        ),
        (
            "let oneArg = fn(a) { a }; oneArg(24);",
            vec![Function(make![GetLocal(0), ReturnValue]), Int(24)],
            make![
                Closure(0, 0),
                SetGlobal(0),
                GetGlobal(0),
                Constant(1),
                Call(1),
                Pop,
            ],
        ),
        (
            "let manyArg = fn(a, b, c) { a; b; c }; manyArg(24, 25, 26);",
            vec![
                Function(make![
                    GetLocal(0),
                    Pop,
                    GetLocal(1),
                    Pop,
                    GetLocal(2),
                    ReturnValue,
                ]),
                Int(24),
                Int(25),
                Int(26),
            ],
            make![
                Closure(0, 0),
                SetGlobal(0),
                GetGlobal(0),
                Constant(1),
                Constant(2),
                Constant(3),
                Call(3),
                Pop,
            ],
        ),
    ]);
}

#[test]
fn test_let_statement_scopes() {
    run_compiler_tests(vec![
        (
            "let num = 55; fn() { num };",
            vec![Int(55), Function(make![GetGlobal(0), ReturnValue])],
            make![Constant(0), SetGlobal(0), Closure(1, 0), Pop],
        ),
        (
            "fn() { let num = 55; num };",
            vec![
                Int(55),
                Function(make![Constant(0), SetLocal(0), GetLocal(0), ReturnValue,]),
            ],
            make![Closure(1, 0), Pop],
        ),
        (
            "fn() { let a = 55; let b = 77; a + b };",
            vec![
                Int(55),
                Int(77),
                Function(make![
                    Constant(0),
                    SetLocal(0),
                    Constant(1),
                    SetLocal(1),
                    GetLocal(0),
                    GetLocal(1),
                    Add,
                    ReturnValue,
                ]),
            ],
            make![Closure(2, 0), Pop],
        ),
    ]);
}

#[test]
fn test_builtins() {
    run_compiler_tests(vec![
        (
            "len([]); push([], 1);",
            vec![Int(1)],
            make![
                GetBuiltin(0),
                Array(0),
                Call(1),
                Pop,
                GetBuiltin(5),
                Array(0),
                Constant(0),
                Call(2),
                Pop,
            ],
        ),
        (
            r#"len("");"#,
            vec![String(Rc::from(""))],
            make![GetBuiltin(0), Constant(0), Call(1), Pop],
        ),
        (
            "fn() { len([]) }",
            vec![Function(make![
                GetBuiltin(0),
                Array(0),
                Call(1),
                ReturnValue,
            ])],
            make![Closure(0, 0), Pop],
        ),
    ]);
}

#[test]
fn test_closures() {
    run_compiler_tests(vec![
        (
            "fn(a) { fn(b) { a + b } }",
            vec![
                Function(make![GetFree(0), GetLocal(0), Add, ReturnValue]),
                Function(make![GetLocal(0), Closure(0, 1), ReturnValue]),
            ],
            make![Closure(1, 0), Pop],
        ),
        (
            r#"
                fn(a) {
                    fn(b) {
                        fn(c) {
                            a + b + c
                        }
                    }
                }"#,
            vec![
                Function(make![
                    GetFree(0),
                    GetFree(1),
                    Add,
                    GetLocal(0),
                    Add,
                    ReturnValue,
                ]),
                Function(make![GetFree(0), GetLocal(0), Closure(0, 2), ReturnValue,]),
                Function(make![GetLocal(0), Closure(1, 1), ReturnValue]),
            ],
            make![Closure(2, 0), Pop],
        ),
        (
            r#"
                let global = 55;
                fn() {
                    let a = 66;
                    fn() {
                        let b = 77;
                        fn() {
                            let c = 88;
                            global + a + b + c;
                        }
                    }
                }"#,
            vec![
                Int(55),
                Int(66),
                Int(77),
                Int(88),
                Function(make![
                    Constant(3),
                    SetLocal(0),
                    GetGlobal(0),
                    GetFree(0),
                    Add,
                    GetFree(1),
                    Add,
                    GetLocal(0),
                    Add,
                    ReturnValue,
                ]),
                Function(make![
                    Constant(2),
                    SetLocal(0),
                    GetFree(0),
                    GetLocal(0),
                    Closure(4, 2),
                    ReturnValue,
                ]),
                Function(make![
                    Constant(1),
                    SetLocal(0),
                    GetLocal(0),
                    Closure(5, 1),
                    ReturnValue,
                ]),
            ],
            make![Constant(0), SetGlobal(0), Closure(6, 0), Pop],
        ),
    ]);
}

#[test]
fn test_recursive_functions() {
    run_compiler_tests(vec![
        (
            r#"
                let countDown = fn(x) { countDown(x - 1); };
                countDown(1);
                "#,
            vec![
                Int(1),
                Function(make![
                    CurrentClosure,
                    GetLocal(0),
                    Constant(0),
                    Sub,
                    Call(1),
                    ReturnValue,
                ]),
                Int(1),
            ],
            make![
                Closure(1, 0),
                SetGlobal(0),
                GetGlobal(0),
                Constant(2),
                Call(1),
                Pop,
            ],
        ),
        (
            r#"
                let wrapper = fn() {
                    let countDown = fn(x) { countDown(x - 1); };
                    countDown(1);
                };
                wrapper();
                "#,
            vec![
                Int(1),
                Function(make![
                    CurrentClosure,
                    GetLocal(0),
                    Constant(0),
                    Sub,
                    Call(1),
                    ReturnValue,
                ]),
                Int(1),
                Function(make![
                    Closure(1, 0),
                    SetLocal(0),
                    GetLocal(0),
                    Constant(2),
                    Call(1),
                    ReturnValue,
                ]),
            ],
            make![Closure(3, 0), SetGlobal(0), GetGlobal(0), Call(0), Pop,],
        ),
    ]);
}
