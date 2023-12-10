#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        util::test_utils::{
            self,
            Constant::{self, Array, Bool, Error, Hash, Int, Null, String},
        },
        vm::{Error as VMError, VM},
    };

    // https://stackoverflow.com/a/27582993
    macro_rules! hash {
        ($($k:expr => $v:expr),* $(,)?) => {{ Hash(core::convert::From::from([$(($k, $v),)*])) }};
    }

    macro_rules! array {
        ($($v:expr),* $(,)?) => {{ Array(core::convert::From::from([$($v,)*])) }};
    }

    macro_rules! string {
        ($v: expr) => {{
            String(Rc::from($v))
        }};
    }

    macro_rules! vm_tests {
        ($name: ident, $($input:expr => $expected:expr),* $(,)?) => {
            #[test]
            fn $name() {
                run_vm_tests(vec![ $(($input, $expected),)* ]);
            }
        };
    }

    type Test<'a> = (&'a str, Constant);

    vm_tests!(
        test_integer_arithmetic,
        "1" => Int(1),
        "2" => Int(2),
        "1 + 2" => Int(3),
        "1 - 2" => Int(-1),
        "1 * 2" => Int(2),
        "4 / 2" => Int(2),
        "50 / 2 * 2 + 10 - 5" => Int(55),
        "5 + 5 + 5 + 5 - 10" => Int(10),
        "2 * 2 * 2 * 2 * 2" => Int(32),
        "5 * 2 + 10" => Int(20),
        "5 + 2 * 10" => Int(25),
        "5 * (2 + 10)" => Int(60),
        "-5" => Int(-5),
        "-10" => Int(-10),
        "-50 + 100 + -50" => Int(0),
        "(5 + 10 * 2 + 15 / 3) * 2 + -10" => Int(50),
    );

    vm_tests!(
        test_boolean_expressions,
        "true" => Bool(true),
        "false" => Bool(false),
        "1 < 2" => Bool(true),
        "1 > 2" => Bool(false),
        "1 < 1" => Bool(false),
        "1 > 1" => Bool(false),
        "1 == 1" => Bool(true),
        "1 != 1" => Bool(false),
        "1 == 2" => Bool(false),
        "1 != 2" => Bool(true),
        "true == true" => Bool(true),
        "false == false" => Bool(true),
        "true == false" => Bool(false),
        "true != false" => Bool(true),
        "false != true" => Bool(true),
        "(1 < 2) == true" => Bool(true),
        "(1 < 2) == false" => Bool(false),
        "(1 > 2) == true" => Bool(false),
        "(1 > 2) == false" => Bool(true),
        "!true" => Bool(false),
        "!false" => Bool(true),
        "!5" => Bool(false),
        "!!true" => Bool(true),
        "!!false" => Bool(false),
        "!!5" => Bool(true),
        "!(if (false) { 5; })" => Bool(true),
    );

    vm_tests!(
        test_conditionals,
        "if (true) { 10 }" => Int(10),
        "if (1) { 10 }" => Int(10),
        "if (1 < 2) { 10 }" => Int(10),
        "if (true) { 10 } else { 20 }" => Int(10),
        "if (false) { 10 } else { 20 } " => Int(20),
        "if (1 < 2) { 10 } else { 20 }" => Int(10),
        "if (1 > 2) { 10 } else { 20 }" => Int(20),
        "if (1 > 2) { 10 }" => Null,
        "if (false) { 10 }" => Null,
        "if ((if (false) { 10 })) { 10 } else { 20 }" => Int(20),
    );

    vm_tests!(
        test_global_test_statements,
        "let one = 1; one" => Int(1),
        "let one = 1; let two = 2; one + two" => Int(3),
        "let one = 1; let two = one + one; one + two" => Int(3),
    );

    vm_tests!(
        test_string_expressions,
        r#""monkey""# => string!("monkey"),
        r#""mon" + "key""# => string!("monkey"),
        r#""mon" + "key" + "banana""# => string!("monkeybanana"),
    );

    vm_tests!(
        test_array_literals,
        "[]" => array![],
        "[1, 2, 3]" => array![Int(1), Int(2), Int(3)],
        "[1 + 2, 3 * 4, 5 + 6]" => array![Int(3), Int(12), Int(11)],
    );

    vm_tests!(
        test_hash_literals,
        "{}" => hash! {},
        "{1: 2, 2: 3}" => hash! { Int(1) => Int(2), Int(2) => Int(3) },
        "{1 + 1: 2 * 2, 3 + 3: 4 * 4}" => hash! { Int(2) => Int(4), Int(6) => Int(16) },
    );

    vm_tests!(
        test_index_expressions,
        "[1, 2, 3][1]" => Int(2),
        "[1, 2, 3][0 + 2]" => Int(3),
        "[[1, 1, 1]][0][0]" => Int(1),
        "[][0]" => Null,
        "[1, 2, 3][99]" => Null,
        "[1][-1]" => Null,
        "{1: 1, 2: 2}[1]" => Int(1),
        "{1: 1, 2: 2}[2]" => Int(2),
        "{1: 1}[0]" => Null,
        "{}[0]" => Null,
    );

    vm_tests!(
        test_calling_functions,
        "let fivePlusTen = fn() { 5 + 10 }; fivePlusTen();" => Int(15),

        r#"
        let one = fn() { 1; };
        let two = fn() { 2; };
        one() + two();"# => Int(3),

        r#"
        let a = fn() { 1 };
        let b = fn() { a() + 1 };
        let c = fn() { b() + 1 };
        c();"# => Int(3),
    );

    vm_tests!(
        test_functions_with_return_statement,
        "let earlyExit = fn() { return 99; 100; }; earlyExit();" => Int(99),
        "let earlyExit = fn() { return 99; return 100; }; earlyExit();" => Int(99),
    );

    vm_tests!(
        test_functions_without_return_value,
        r#"
        let noReturn = fn() { };
        noReturn();"# => Null,

        r#"
        let noReturn = fn() { };
        let noReturnTwo = fn() { noReturn(); };
        noReturn();
        noReturnTwo();"# => Null,
    );

    vm_tests!(
        test_first_class_functions,
        r#"
        let returnsOne = fn() { 1; };
        let returnsOneReturner = fn() { returnsOne; };
        returnsOneReturner()();"# => Int(1),

        r#"
        let returnsOneReturner = fn() {
            let returnsOne = fn() { 1; };
            returnsOne;
        };
        returnsOneReturner()();"# => Int(1),
    );

    vm_tests!(
        test_calling_functions_with_bindings,
        "let one = fn() { let one = 1; one }; one();" => Int(1),

        r#"
        let oneAndTwo = fn() {
            let one = 1;
            let two = 2;
            one + two;
        };
        oneAndTwo();"# => Int(3),

        r#"
        let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
        let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
        oneAndTwo() + threeAndFour();"# => Int(10),

        r#"
        let firstFoobar = fn() { let foobar = 50; foobar; };
        let secondFoobar = fn() { let foobar = 100; foobar; };
        firstFoobar() + secondFoobar(); "# => Int(150),

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
        minusOne() + minusTwo();"# => Int(97),
    );

    vm_tests!(
        test_calling_functions_with_arguments_and_bindings,
        "let identity = fn(a) { a; }; identity(4);" => Int(4),
        "let sum = fn(a, b) { a + b; }; sum(1, 2);" => Int(3),

        r#"
        let sum = fn(a,b) {
            let c = a + b;
            c;
        };
        sum(1, 2);"# => Int(3),

        r#"
        let sum = fn(a, b) {
            let c = a + b;
            c;
        };
        sum(1, 2) + sum(3, 4);"# => Int(10),

        r#"
        let sum = fn(a, b) {
            let c = a + b;
            c;
        };
        let outer = fn() {
            sum(1, 2) + sum(3, 4);
        };
        outer();"# => Int(10),

        r#"
        let globalNum = 10;
        let sum = fn(a, b) {
            let c = a + b;
            c + globalNum;
        };
        let outer = fn() {
            sum(1, 2) + sum(3, 4) + globalNum;
        };
        outer() + globalNum;"# => Int(50),
    );

    vm_tests!(
        test_calling_functions_with_wrong_arguments,
        "fn() { 1; }(1);" => Error(VMError::WrongArguments{ want: 0, got: 1 }),
        "fn(a) { a; }();" => Error(VMError::WrongArguments{ want: 1, got: 0 }),
        "fn(a, b) { a + b; }(1);" => Error(VMError::WrongArguments{ want: 2, got: 1 }),
    );

    fn run_vm_tests(tests: Vec<Test>) {
        for (input, expected) in tests {
            let bytecode = test_utils::compile_program(input);
            let disassembly = crate::code::Disassembled(bytecode.instructions.clone());
            let vm = match VM::new(bytecode).run() {
                Ok(vm) => vm,
                Err(err) => match expected {
                    Error(e) if e == err => continue,
                    _ => panic!("vm error: {err:?}\ninput: {input}\n{disassembly}"),
                },
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
