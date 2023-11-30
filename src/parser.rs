use crate::ast::{
    Binary::{Add, Div, Eq, Mul, Neq, Sub, GT, LT},
    BlockStatement, Expression,
    Literal::{Array, Boolean, Function, Hash, Identifier, If, Integer, String},
    Operator::{self, Binary, Call, Index, Unary},
    Program,
    Statement::{self, Let, Return},
    Unary::{Neg, Not},
};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, multispace1},
    combinator::{cut, map, map_res, opt, recognize, verify},
    error::{context, VerboseError},
    multi::{many0, many0_count, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    Parser,
};

type IResult<'a, O1> = nom::IResult<&'a str, O1, VerboseError<&'a str>>;

macro_rules! wrapper {
    ($name: ident, $left: expr, $right: expr) => {
        fn $name<'a, O1, F>(inner: F) -> impl FnMut(&'a str) -> IResult<O1>
        where
            F: Parser<&'a str, O1, VerboseError<&'a str>>,
        {
            delimited($left, inner, $right)
        }
    };
    ($name: ident, $sep: expr) => {
        wrapper!($name, $sep, $sep);
    };
}

wrapper!(spaced, multispace0);
wrapper!(quotes, char('"'));
wrapper!(parens, char('('), char(')'));
wrapper!(squarely, char('['), char(']'));
wrapper!(squirly, char('{'), char('}'));

fn keyword0<'a, O1, F>(kw: &'static str, inner: F) -> impl FnMut(&'a str) -> IResult<O1>
where
    F: Parser<&'a str, O1, VerboseError<&'a str>>,
{
    preceded(terminated(tag(kw), multispace0), inner)
}

fn keyword_stmt<'a, O1, F>(kw: &'static str, inner: F) -> impl FnMut(&'a str) -> IResult<O1>
where
    F: Parser<&'a str, O1, VerboseError<&'a str>>,
{
    delimited(terminated(tag(kw), multispace1), inner, opt(char(';')))
}

fn identifier(i: &str) -> IResult<&str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(i)
}

fn literal(i: &str) -> IResult<Expression> {
    map(
        alt((
            |i| {
                let (i, cond) = context("if expr", keyword0("if", cut(parens(expr))))(i)?;
                let (i, consequence) = context("consequence", spaced(squirly(cut(block))))(i)?;
                let (i, alternative) = opt(spaced(context(
                    "else",
                    keyword0("else", squirly(cut(block))),
                )))(i)?;
                Ok((i, If(Box::new(cond), consequence, alternative)))
            },
            |i| {
                let (i, args) = context(
                    "function",
                    keyword0(
                        "fn",
                        cut(parens(separated_list0(char(','), spaced(identifier)))),
                    ),
                )(i)?;
                let (i, body) = spaced(squirly(block))(i)?;
                Ok((i, Function(args, body)))
            },
            map(squarely(separated_list0(char(','), expr)), Array),
            |i| {
                let (i, pairs) = squirly(separated_list0(
                    char(','),
                    separated_pair(expr, char(':'), expr),
                ))(i)?;
                Ok((i, Hash(pairs.into_iter().collect())))
            },
            map_res(digit1, |int: &str| int.parse::<i64>().map(Integer)),
            map(recognize(alt((tag("true"), tag("false")))), |b| {
                Boolean(b == "true")
            }),
            // No escaped characters, classic
            |i| {
                let (i, s) = quotes(verify(is_not("\""), |s: &str| !s.is_empty()))(i)?;
                Ok((i, String(s)))
            },
            map(identifier, Identifier),
        )),
        Expression::Literal,
    )(i)
}

fn index(i: &str) -> IResult<Expression> {
    let (i, term) = spaced(alt((literal, parens(expr))))(i)?;
    let (i, args) = many0(spaced(squarely(map(expr, Index))))(i)?;
    Ok((i, fold_exprs(term, args)))
}

fn call(i: &str) -> IResult<Expression> {
    let (i, term) = index(i)?;
    let (i, args) = many0(spaced(parens(map(separated_list0(char(','), expr), Call))))(i)?;
    Ok((i, fold_exprs(term, args)))
}

fn prefix(i: &str) -> IResult<Expression> {
    let (i, mut prefixes) = many0(alt((
        map(spaced(char('-')), |_| Unary(Neg)),
        map(spaced(char('!')), |_| Unary(Not)),
    )))(i)?;
    let (i, term) = call(i)?;
    prefixes.reverse();
    Ok((i, fold_exprs(term, prefixes)))
}

fn mul_div(i: &str) -> IResult<Expression> {
    let (i, initial) = prefix(i)?;
    let (i, remainder) = many0(alt((
        map(spaced(preceded(char('*'), prefix)), |mul| Binary(Mul, mul)),
        map(spaced(preceded(char('/'), prefix)), |div| Binary(Div, div)),
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

fn sum(i: &str) -> IResult<Expression> {
    let (i, initial) = mul_div(i)?;
    let (i, remainder) = many0(alt((
        map(spaced(preceded(char('+'), mul_div)), |add| Binary(Add, add)),
        map(spaced(preceded(char('-'), mul_div)), |sub| Binary(Sub, sub)),
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

fn lt_gt(i: &str) -> IResult<Expression> {
    let (i, initial) = sum(i)?;
    let (i, remainder) = many0(alt((
        map(spaced(preceded(char('<'), sum)), |lt| Binary(LT, lt)),
        map(spaced(preceded(char('>'), sum)), |gt| Binary(GT, gt)),
    )))(i)?;
    Ok((i, fold_exprs(initial, remainder)))
}

fn expr(i: &str) -> IResult<Expression> {
    let (i, initial) = lt_gt(i)?;
    let (i, remainder) = many0(alt((
        map(spaced(preceded(tag("=="), lt_gt)), |eq| Binary(Eq, eq)),
        map(spaced(preceded(tag("!="), lt_gt)), |neq| Binary(Neq, neq)),
    )))(i)?;
    Ok((i, fold_exprs(initial, remainder)))
}

fn fold_exprs<'a>(initial: Expression<'a>, remainder: Vec<Operator<'a>>) -> Expression<'a> {
    remainder.into_iter().fold(initial, |acc, op| op.fold(acc))
}

fn block(i: &str) -> IResult<BlockStatement> {
    map(
        many0(spaced(alt((
            map(
                keyword_stmt("let", separated_pair(identifier, spaced(char('=')), expr)),
                |(ident, expr)| Let(ident, expr),
            ),
            map(keyword_stmt("return", expr), Return),
            map(terminated(expr, opt(char(';'))), Statement::Expression),
        )))),
        BlockStatement,
    )(i)
}

/// # Errors
/// Nom parser error
pub fn program(i: &str) -> IResult<Program> {
    map(block, Program)(i)
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        self,
        Binary::{Add, Div, Eq, Mul, Neq, Sub, GT, LT},
        Expression::{Call, Index, Infix, Literal, Prefix},
        Literal::{Array, Boolean, Function, Hash, Identifier, If, Integer, String},
        Statement::{self, Expression, Let, Return},
        Unary::{Neg, Not},
    };

    macro_rules! assert_matches {
        ($statement: expr, $val: pat $(if $guard:expr)? $(,)?) => {
            assert!(matches!($statement, $val $(if $guard)?))
        };
    }

    macro_rules! assert_literal {
        ($statement: expr, $val: pat $(if $guard:expr)? $(,)?) => {
            assert_matches!($statement, Expression(Literal($val)) $(if $guard)?)
        };
    }

    macro_rules! infix_expr_matches {
        ($expr: expr, $left: pat, $operator: pat, $right: pat) => {
            matches!($expr, Infix(left, $operator, right) if matches!(left.as_ref(), Literal($left)) && matches!(right.as_ref(), Literal($right)))
        };
    }

    macro_rules! assert_infix_expr {
        ($expr: expr, $left: pat, $operator: pat, $right: pat) => {
            assert!(infix_expr_matches!($expr, $left, $operator, $right));
        };
    }

    macro_rules! assert_infix {
        ($statement: expr, $left: pat, $operator: pat, $right: pat) => {
            if let Expression(expr) = $statement {
                assert_infix_expr!(expr, $left, $operator, $right);
            } else {
                panic!("not an expression");
            }
        };
    }

    macro_rules! assert_call {
        ($statement: expr, $fn: literal, $len: literal, $asserts: expr) => {
            if let Expression(Call(function, args)) = $statement {
                assert_matches!(function.as_ref(), Literal(Identifier($fn)));
                assert_eq!(args.len(), $len);
                $asserts(args);
            } else {
                panic!("not a call");
            }
        };
    }

    macro_rules! assert_fn {
        ($statement: expr, $argc: literal, $len: literal, $asserts: expr) => {
            if let Expression(Literal(Function(args, body))) = $statement {
                assert_eq!(args.len(), $argc);
                assert_eq!(body.0.len(), $len);
                $asserts(args, body);
            } else {
                panic!("not a function");
            }
        };
    }

    fn parse_program(input: &str, len: usize) -> Vec<Statement> {
        let program = super::program(input).expect("must parse").1 .0 .0;
        assert_eq!(program.len(), len);
        program
    }

    #[test]
    fn test_literals() {
        let program = parse_program(r#"5; true; false; foo; "hello world";"#, 5);
        assert_literal!(&program[0], Integer(5));
        assert_literal!(&program[1], Boolean(true));
        assert_literal!(&program[2], Boolean(false));
        assert_literal!(&program[3], Identifier("foo"));
        assert_literal!(&program[4], String("hello world"));
    }

    #[test]
    fn test_prefix_expressions() {
        macro_rules! assert_prefix {
            ($statement: expr, $operator: pat, $right: pat) => {
                assert_matches!($statement, Expression(Prefix($operator, h)) if matches!(h.as_ref(), Literal($right)));
            };
        }
        let program = parse_program("!5; -15; !true; !false;", 4);
        assert_prefix!(&program[0], Not, Integer(5));
        assert_prefix!(&program[1], Neg, Integer(15));
        assert_prefix!(&program[2], Not, Boolean(true));
        assert_prefix!(&program[3], Not, Boolean(false));
    }

    #[test]
    fn test_call_expression() {
        let program = parse_program("add(1, 2 * 3, 4 + 5);", 1);
        assert_call!(&program[0], "add", 3, |args: &Vec<ast::Expression>| {
            assert_matches!(&args[0], Literal(Integer(1)));
            assert_infix_expr!(&args[1], Integer(2), Mul, Integer(3));
            assert_infix_expr!(&args[2], Integer(4), Add, Integer(5));
        });
    }

    #[test]
    fn test_call_parameter_parsing() {
        let program = parse_program(r#"add(); add(1); mult(1, 2 * 3, 4 + 5);"#, 3);
        assert_call!(&program[0], "add", 0, |_| {});
        assert_call!(&program[1], "add", 1, |args: &Vec<ast::Expression>| {
            assert_matches!(&args[0], Literal(Integer(1)));
        });
        assert_call!(&program[2], "mult", 3, |args: &Vec<ast::Expression>| {
            assert_matches!(&args[0], Literal(Integer(1)));
            assert_infix_expr!(&args[1], Integer(2), Mul, Integer(3));
            assert_infix_expr!(&args[2], Integer(4), Add, Integer(5));
        });
    }

    #[test]
    fn test_index_expression() {
        let program = parse_program("myArray[1 + 1];", 1);
        if let Expression(Index { left, index }) = &program[0] {
            assert_matches!(left.as_ref(), Literal(Identifier("myArray")));
            assert_infix_expr!(index.as_ref(), Integer(1), Add, Integer(1));
        } else {
            panic!("not an index");
        }
    }

    #[test]
    fn test_infix_expressions() {
        let program = parse_program(
            r#"
            3 + 10;
            5 - 5;
            5 * 5;
            5 / 5;
            5 > 5;
            5 < 5;
            5 == 5;
            5 != 5;
            true == true;
            false == false;
            true != false;
            "#,
            11,
        );
        assert_infix!(&program[0], Integer(3), Add, Integer(10));
        assert_infix!(&program[1], Integer(5), Sub, Integer(5));
        assert_infix!(&program[2], Integer(5), Mul, Integer(5));
        assert_infix!(&program[3], Integer(5), Div, Integer(5));
        assert_infix!(&program[4], Integer(5), GT, Integer(5));
        assert_infix!(&program[5], Integer(5), LT, Integer(5));
        assert_infix!(&program[6], Integer(5), Eq, Integer(5));
        assert_infix!(&program[7], Integer(5), Neq, Integer(5));
        assert_infix!(&program[8], Boolean(true), Eq, Boolean(true));
        assert_infix!(&program[9], Boolean(false), Eq, Boolean(false));
        assert_infix!(&program[10], Boolean(true), Neq, Boolean(false));
    }

    #[test]
    fn test_let_statement() {
        let program = parse_program("let x = 5; let y = true; let foobar = y;", 3);
        assert_matches!(&program[0], Let("x", Literal(Integer(5))));
        assert_matches!(&program[1], Let("y", Literal(Boolean(true))));
        assert_matches!(&program[2], Let("foobar", Literal(Identifier("y"))));
    }

    #[test]
    fn test_return_statement() {
        let program = parse_program("return 5; return true; return foobar; ", 3);
        assert_matches!(&program[0], Return(Literal(Integer(5))));
        assert_matches!(&program[1], Return(Literal(Boolean(true))));
        assert_matches!(&program[2], Return(Literal(Identifier("foobar"))));
        let program = parse_program("return 5", 1);
        assert_matches!(&program[0], Return(Literal(Integer(5))));
    }

    #[test]
    fn test_if_expression() {
        let program = parse_program("if (x < y) { x }; if (x < y) { x } else { y };", 2);
        assert_matches!(
            &program[0],
            Expression(Literal(If (
                cond,
                consequence,
                None
            ))) if {
                matches!(consequence.0.len(), 1) &&
                infix_expr_matches!(cond.as_ref(), Identifier("x"), LT, Identifier("y")) &&
                matches!(&consequence.0[0], Expression(Literal(Identifier("x"))))
            }
        );
        assert_matches!(
            &program[1],
            Expression(Literal(If (
                cond,
                consequence,
                Some(alt),
            ))) if {
                matches!(consequence.0.len(), 1) &&
                infix_expr_matches!(cond.as_ref(), Identifier("x"), LT, Identifier("y")) &&
                matches!(&consequence.0[0], Expression(Literal(Identifier("x")))) &&
                matches!(&alt.0.len(), 1) &&
                matches!(&alt.0[0], Expression(Literal(Identifier("y"))))
            }
        );
    }

    #[test]
    fn test_function_literal() {
        let program = parse_program("fn(x, y) { x + y; };", 1);
        assert_fn!(
            &program[0],
            2,
            1,
            |args: &Vec<&str>, body: &ast::BlockStatement| {
                assert_eq!(args, &vec!["x", "y"]);
                assert_infix!(&body.0[0], Identifier("x"), Add, Identifier("y"));
            }
        );
    }

    #[test]
    fn test_function_parameter_parsing() {
        let program = parse_program("fn(){}; fn(x){}; fn(x,y,z){};", 3);
        assert_fn!(&program[0], 0, 0, |_, _| {});
        assert_fn!(&program[1], 1, 0, |args: &Vec<&str>, _| {
            assert_eq!(args, &vec!["x"]);
        });
        assert_fn!(&program[2], 3, 0, |args: &Vec<&str>, _| {
            assert_eq!(args, &vec!["x", "y", "z"]);
        });
    }

    #[test]
    fn test_array_literal() {
        let program = parse_program("[]; [1, 2 * 2, 3 + 3]", 2);
        assert_literal!(&program[0], Array(arr) if arr.is_empty());
        assert_literal!(&program[1], Array(arr) if {
            arr.len() == 3 &&
            matches!(&arr[0], Literal(Integer(1))) &&
            infix_expr_matches!(&arr[1], Integer(2), Mul, Integer(2)) &&
            infix_expr_matches!(&arr[2], Integer(3), Add, Integer(3))
        });
    }

    #[test]
    fn test_hash_literal() {
        let program = parse_program(
            r#"
            {};
            {"one": 1, "two": 2, "three": 3 };
            {"one": 0 + 1, "two": 10 - 8, "three": 15 / 5 };
            "#,
            3,
        );
        assert_literal!(&program[0], Hash(h) if h.is_empty());
        assert_literal!(&program[1], Hash(arr) if {
            arr.len() == 3 &&
            matches!(&arr[&Literal(String("one"))], Literal(Integer(1))) &&
            matches!(&arr[&Literal(String("two"))], Literal(Integer(2))) &&
            matches!(&arr[&Literal(String("three"))], Literal(Integer(3)))
        });
        assert_literal!(&program[2], Hash(arr) if {
            arr.len() == 3 &&
            infix_expr_matches!(&arr[&Literal(String("one"))], Integer(0), Add, Integer(1)) &&
            infix_expr_matches!(&arr[&Literal(String("two"))], Integer(10), Sub, Integer(8)) &&
            infix_expr_matches!(&arr[&Literal(String("three"))], Integer(15), Div, Integer(5))
        });
    }

    #[test]
    fn test_operator_precedence() {
        let cases = vec![
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4", "(3 + 4)"),
            ("-5 * 5", "((-5) * 5)"),
            ("5 * -5", "(5 * (-5))"),
            ("-50 + 100 + -50", "(((-50) + 100) + (-50))"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (case, want) in cases {
            let program = parse_program(case, 1);
            match &program[0] {
                Expression(expr) => {
                    let prog = expr.to_string();
                    if prog != want {
                        println!("case: {case}");
                        println!("got: {prog}");
                        println!("want: {want}");
                    }
                    assert!(prog == want)
                }
                _ => panic!("not an expression"),
            }
        }
    }
}
