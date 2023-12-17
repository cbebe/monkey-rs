use std::rc::Rc;

use crate::{
    code::{
        self, Instructions,
        Opcode::{
            Add, Array, Bang, Constant, Div, Equal, False, GetGlobal, GreaterThan, Hash, Index,
            Jump, JumpNotTruthy, Minus, Mul, NotEqual, Null, Pop, SetGlobal, Sub, True,
        },
    },
    util::test_utils::{
        self, compile_program, test_constants, test_instructions,
        Constant::{Int, String},
    },
};

type Test<'a> = (&'a str, Vec<test_utils::Constant>, Vec<Instructions>);

pub mod func {
    macro_rules! make {
        ($($x:expr),+ $(,)?) => {{
            let ops = vec![$($x),+];
            let inst: Vec<Instructions> = ops.into_iter().map(code::make).collect();
            inst
        }};
    }
    pub(crate) use make;
}

use func::make;

pub fn run_compiler_tests(tests: Vec<Test>) {
    for (input, constants, instructions) in tests {
        let bytecode = compile_program(input);
        assert!(
            test_instructions(instructions.clone(), bytecode.instructions.clone()),
            "failed test for input {}.\nwant:\n{}\ngot:\n{}",
            &input,
            code::Disassembled(instructions.into_iter().flatten().collect::<Instructions>()),
            code::Disassembled(bytecode.instructions)
        );
        if let Err(err) = test_constants(&constants, &bytecode.constants) {
            panic!("failed test for input {}.\n{err}", &input);
        }
    }
}

#[test]
fn test_integer_arithmetic() {
    run_compiler_tests(vec![
        (
            "1 + 2",
            vec![Int(1), Int(2)],
            make![Constant(0), Constant(1), Add, Pop],
        ),
        (
            "1; 2",
            vec![Int(1), Int(2)],
            make![Constant(0), Pop, Constant(1), Pop],
        ),
        (
            "1 - 2",
            vec![Int(1), Int(2)],
            make![Constant(0), Constant(1), Sub, Pop],
        ),
        (
            "1 * 2",
            vec![Int(1), Int(2)],
            make![Constant(0), Constant(1), Mul, Pop],
        ),
        (
            "2 / 1",
            vec![Int(2), Int(1)],
            make![Constant(0), Constant(1), Div, Pop],
        ),
        ("-1", vec![Int(1)], make![Constant(0), Minus, Pop]),
    ]);
}

#[test]
fn test_boolean_expressions() {
    run_compiler_tests(vec![
        ("true", vec![], make![True, Pop]),
        ("false", vec![], make![False, Pop]),
        (
            "1 > 2",
            vec![Int(1), Int(2)],
            make![Constant(0), Constant(1), GreaterThan, Pop],
        ),
        (
            "1 < 2",
            vec![Int(2), Int(1)],
            make![Constant(0), Constant(1), GreaterThan, Pop],
        ),
        (
            "1 == 2",
            vec![Int(1), Int(2)],
            make![Constant(0), Constant(1), Equal, Pop],
        ),
        (
            "1 != 2",
            vec![Int(1), Int(2)],
            make![Constant(0), Constant(1), NotEqual, Pop],
        ),
        ("true == false", vec![], make![True, False, Equal, Pop]),
        ("true != false", vec![], make![True, False, NotEqual, Pop]),
        ("!true", vec![], make![True, Bang, Pop]),
    ]);
}

#[test]
fn test_conditionals() {
    run_compiler_tests(vec![
        (
            "if (true) { 10 }; 3333;",
            vec![Int(10), Int(3333)],
            make![
                // 0000
                True,
                // 0001
                JumpNotTruthy(10),
                // 0004
                Constant(0),
                // 0007
                Jump(11),
                // 0010
                Null,
                // 0011
                Pop,
                // 0012
                Constant(1),
                // 0015
                Pop,
            ],
        ),
        (
            "if (true) { 10 } else { 20 }; 3333;",
            vec![Int(10), Int(20), Int(3333)],
            make![
                // 0000
                True,
                // 0001
                JumpNotTruthy(10),
                // 0004
                Constant(0),
                // 0007
                Jump(13),
                // 0010
                Constant(1),
                // 0013
                Pop,
                // 0014
                Constant(2),
                // 0017
                Pop,
            ],
        ),
    ]);
}

#[test]
fn test_global_let_statements() {
    run_compiler_tests(vec![
        (
            "let one = 1; let two = 2;",
            vec![Int(1), Int(2)],
            make![Constant(0), SetGlobal(0), Constant(1), SetGlobal(1)],
        ),
        (
            "let one = 1; one;",
            vec![Int(1)],
            make![Constant(0), SetGlobal(0), GetGlobal(0), Pop],
        ),
        (
            "let one = 1; let two = one; two",
            vec![Int(1)],
            make![
                Constant(0),
                SetGlobal(0),
                GetGlobal(0),
                SetGlobal(1),
                GetGlobal(1),
                Pop,
            ],
        ),
    ]);
}

#[test]
fn test_string_expressions() {
    run_compiler_tests(vec![
        (
            r#""monkey""#,
            vec![String(Rc::from("monkey"))],
            make![Constant(0), Pop],
        ),
        (
            r#""mon" + "key""#,
            vec![String(Rc::from("mon")), String(Rc::from("key"))],
            make![Constant(0), Constant(1), Add, Pop],
        ),
    ]);
}

#[test]
fn test_array_literals() {
    run_compiler_tests(vec![
        ("[]", vec![], make![Array(0), Pop]),
        (
            "[1, 2, 3]",
            vec![Int(1), Int(2), Int(3)],
            make![Constant(0), Constant(1), Constant(2), Array(3), Pop],
        ),
        (
            "[1 + 2, 3 - 4, 5 * 6]",
            vec![Int(1), Int(2), Int(3), Int(4), Int(5), Int(6)],
            make![
                Constant(0),
                Constant(1),
                Add,
                Constant(2),
                Constant(3),
                Sub,
                Constant(4),
                Constant(5),
                Mul,
                Array(3),
                Pop,
            ],
        ),
    ]);
}

#[test]
fn test_hash_literals() {
    run_compiler_tests(vec![
        ("{}", vec![], make![Hash(0), Pop]),
        (
            "{1: 2, 3: 4, 5: 6}",
            vec![Int(1), Int(2), Int(3), Int(4), Int(5), Int(6)],
            make![
                Constant(0),
                Constant(1),
                Constant(2),
                Constant(3),
                Constant(4),
                Constant(5),
                Hash(6),
                Pop,
            ],
        ),
        (
            "{1: 2 + 3, 4: 5 * 6}",
            vec![Int(1), Int(2), Int(3), Int(4), Int(5), Int(6)],
            make![
                Constant(0),
                Constant(1),
                Constant(2),
                Add,
                Constant(3),
                Constant(4),
                Constant(5),
                Mul,
                Hash(4),
                Pop,
            ],
        ),
    ]);
}

#[test]
fn test_index_expresssions() {
    run_compiler_tests(vec![
        (
            "[1, 2, 3][1 + 1]",
            vec![Int(1), Int(2), Int(3), Int(1), Int(1)],
            make![
                Constant(0),
                Constant(1),
                Constant(2),
                Array(3),
                Constant(3),
                Constant(4),
                Add,
                Index,
                Pop,
            ],
        ),
        (
            "{1: 2}[2 - 1]",
            vec![Int(1), Int(2), Int(2), Int(1)],
            make![
                Constant(0),
                Constant(1),
                Hash(2),
                Constant(2),
                Constant(3),
                Sub,
                Index,
                Pop,
            ],
        ),
    ]);
}
