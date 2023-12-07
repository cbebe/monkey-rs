#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        util::test_utils::{
            self,
            Constant::{self, Array, Bool, Hash, Int, Null, String},
        },
        vm::VM,
    };

    // https://stackoverflow.com/a/27582993
    macro_rules! collection {
        // map-like
        ($($k:expr => $v:expr),* $(,)?) => {{
            core::convert::From::from([$(($k, $v),)*])
        }};
        // set-like
        ($($v:expr),* $(,)?) => {{
            core::convert::From::from([$($v,)*])
        }};
    }

    type Test<'a> = (&'a str, Constant);

    #[test]
    fn test_integer_arithmetic() {
        run_vm_tests(vec![
            ("1", Int(1)),
            ("2", Int(2)),
            ("1 + 2", Int(3)),
            ("1 - 2", Int(-1)),
            ("1 * 2", Int(2)),
            ("4 / 2", Int(2)),
            ("50 / 2 * 2 + 10 - 5", Int(55)),
            ("5 + 5 + 5 + 5 - 10", Int(10)),
            ("2 * 2 * 2 * 2 * 2", Int(32)),
            ("5 * 2 + 10", Int(20)),
            ("5 + 2 * 10", Int(25)),
            ("5 * (2 + 10)", Int(60)),
            ("-5", Int(-5)),
            ("-10", Int(-10)),
            ("-50 + 100 + -50", Int(0)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Int(50)),
        ]);
    }

    #[test]
    fn test_boolean_expressions() {
        run_vm_tests(vec![
            ("true", Bool(true)),
            ("false", Bool(false)),
            ("1 < 2", Bool(true)),
            ("1 > 2", Bool(false)),
            ("1 < 1", Bool(false)),
            ("1 > 1", Bool(false)),
            ("1 == 1", Bool(true)),
            ("1 != 1", Bool(false)),
            ("1 == 2", Bool(false)),
            ("1 != 2", Bool(true)),
            ("true == true", Bool(true)),
            ("false == false", Bool(true)),
            ("true == false", Bool(false)),
            ("true != false", Bool(true)),
            ("false != true", Bool(true)),
            ("(1 < 2) == true", Bool(true)),
            ("(1 < 2) == false", Bool(false)),
            ("(1 > 2) == true", Bool(false)),
            ("(1 > 2) == false", Bool(true)),
            ("!true", Bool(false)),
            ("!false", Bool(true)),
            ("!5", Bool(false)),
            ("!!true", Bool(true)),
            ("!!false", Bool(false)),
            ("!!5", Bool(true)),
            ("!(if (false) { 5; })", Bool(true)),
        ]);
    }

    #[test]
    fn test_conditionals() {
        run_vm_tests(vec![
            ("if (true) { 10 }", Int(10)),
            ("if (1) { 10 }", Int(10)),
            ("if (1 < 2) { 10 }", Int(10)),
            ("if (true) { 10 } else { 20 }", Int(10)),
            ("if (false) { 10 } else { 20 } ", Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", Int(10)),
            ("if (1 > 2) { 10 } else { 20 }", Int(20)),
            ("if (1 > 2) { 10 }", Null),
            ("if (false) { 10 }", Null),
            ("if ((if (false) { 10 })) { 10 } else { 20 }", Int(20)),
        ]);
    }

    #[test]
    fn test_global_test_statements() {
        run_vm_tests(vec![
            ("let one = 1; one", Int(1)),
            ("let one = 1; let two = 2; one + two", Int(3)),
            ("let one = 1; let two = one + one; one + two", Int(3)),
        ]);
    }

    #[test]
    fn test_string_expressions() {
        run_vm_tests(vec![
            (r#""monkey""#, String(Rc::from("monkey"))),
            (r#""mon" + "key""#, String(Rc::from("monkey"))),
            (
                r#""mon" + "key" + "banana""#,
                String(Rc::from("monkeybanana")),
            ),
        ]);
    }

    #[test]
    fn test_array_literals() {
        run_vm_tests(vec![
            ("[]", Array(vec![])),
            ("[1, 2, 3]", Array(vec![Int(1), Int(2), Int(3)])),
            (
                "[1 + 2, 3 * 4, 5 + 6]",
                Array(vec![Int(3), Int(12), Int(11)]),
            ),
        ]);
    }

    #[test]
    fn test_hash_literals() {
        run_vm_tests(vec![
            ("{}", Hash(collection! {})),
            (
                "{1: 2, 2: 3}",
                Hash(collection! { Int(1) => Int(2), Int(2) => Int(3) }),
            ),
            (
                "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                Hash(collection! { Int(2) => Int(4), Int(6) => Int(16) }),
            ),
        ]);
    }

    #[test]
    fn test_index_expressions() {
        run_vm_tests(vec![
            ("[1, 2, 3][1]", Int(2)),
            ("[1, 2, 3][0 + 2]", Int(3)),
            ("[[1, 1, 1]][0][0]", Int(1)),
            ("[][0]", Null),
            ("[1, 2, 3][99]", Null),
            ("[1][-1]", Null),
            ("{1: 1, 2: 2}[1]", Int(1)),
            ("{1: 1, 2: 2}[2]", Int(2)),
            ("{1: 1}[0]", Null),
            ("{}[0]", Null),
        ]);
    }

    #[test]
    fn test_calling_functions() {
        run_vm_tests(vec![
            ("let fivePlusTen = fn() { 5 + 10 }; fivePlusTen();", Int(15)),
            (
                r#"
let one = fn() { 1; };
let two = fn() { 2; };
one() + two();
                "#,
                Int(3),
            ),
            (
                r#"
let a = fn() { 1 };
let b = fn() { a() + 1 };
let c = fn() { b() + 1 };
c();
                "#,
                Int(3),
            ),
        ]);
    }

    #[test]
    fn test_functions_with_return_statement() {
        run_vm_tests(vec![
            (
                "let earlyExit = fn() { return 99; 100; }; earlyExit();",
                Int(99),
            ),
            (
                "let earlyExit = fn() { return 99; return 100; }; earlyExit();",
                Int(99),
            ),
        ]);
    }

    #[test]
    fn test_functions_without_return_value() {
        run_vm_tests(vec![
            (
                r#"
let noReturn = fn() { };
noReturn();
                "#,
                Null,
            ),
            (
                r#"
let noReturn = fn() { };
let noReturnTwo = fn() { noReturn(); };
noReturn();
noReturnTwo();
                "#,
                Null,
            ),
        ]);
    }

    #[test]
    fn test_first_class_functions() {
        run_vm_tests(vec![
            (
                r#"
let returnsOne = fn() { 1; };
let returnsOneReturner = fn() { returnsOne; };
returnsOneReturner()();
            "#,
                Int(1),
            ),
            (
                r#"
let returnsOneReturner = fn() {
    let returnsOne = fn() { 1; };
    returnsOne;
};
returnsOneReturner()();"#,
                Int(1),
            ),
        ]);
    }

    #[test]
    fn test_calling_functions_with_bindings() {
        run_vm_tests(vec![
            (
                r#"
let one = fn() { let one = 1; one };
one();
                "#,
                Int(1),
            ),
            (
                r#"
let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
oneAndTwo();
                "#,
                Int(3),
            ),
            (
                r#"
let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
oneAndTwo() + threeAndFour();
                "#,
                Int(10),
            ),
            (
                r#"
let firstFoobar = fn() { let foobar = 50; foobar; };
let secondFoobar = fn() { let foobar = 100; foobar; };
firstFoobar() + secondFoobar();
                "#,
                Int(150),
            ),
            (
                r#"
let globalSeed = 50;
let minusOne = fn() {
    let num = 1;
    globalSeed - num;
};
let minusTwo = fn() {
    let num = 2;
    globalSeed - num;
};
minusOne() + minusTwo();
                "#,
                Int(97),
            ),
        ])
    }

    fn run_vm_tests(tests: Vec<Test>) {
        for (input, expected) in tests {
            let bytecode = test_utils::compile_program(input);
            let disassembly = crate::code::Disassembled(bytecode.instructions.clone());
            let vm = match VM::new(bytecode).run() {
                Ok(vm) => vm,
                Err(err) => panic!("vm error: {err:?}\ninput: {input}\n{disassembly}"),
            };
            let got = vm
                .last_popped()
                .unwrap_or_else(|| panic!("no stack value\ninput: {input}\n{disassembly}"));
            if let Err(err) = test_utils::test_object(got, &expected) {
                panic!("failed test\ninput: {input}\n{err}")
            }
        }
    }
}
