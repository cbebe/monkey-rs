mod ast;

use ast::{BlockStatement, Expression, Literal, Operator, Program, Statement};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, multispace1},
    combinator::{map, map_res, opt, recognize, verify},
    error::VerboseError,
    multi::{many0, many0_count, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    IResult, Parser,
};

fn spaced<'a, O1, F>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O1, VerboseError<&'a str>>
where
    F: Parser<&'a str, O1, VerboseError<&'a str>>,
{
    delimited(multispace0, inner, multispace0)
}

fn parens<'a, O1, F>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O1, VerboseError<&'a str>>
where
    F: Parser<&'a str, O1, VerboseError<&'a str>>,
{
    delimited(char('('), inner, char(')'))
}

fn squarely<'a, O1, F>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O1, VerboseError<&'a str>>
where
    F: Parser<&'a str, O1, VerboseError<&'a str>>,
{
    delimited(char('['), inner, char(']'))
}

fn squirly<'a, O1, F>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O1, VerboseError<&'a str>>
where
    F: Parser<&'a str, O1, VerboseError<&'a str>>,
{
    delimited(char('{'), inner, char('}'))
}

fn keyword0<'a, O1, F>(
    kw: &'static str,
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O1, VerboseError<&'a str>>
where
    F: Parser<&'a str, O1, VerboseError<&'a str>>,
{
    preceded(terminated(tag(kw), multispace0), inner)
}

fn keyword1<'a, O1, F>(
    kw: &'static str,
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O1, VerboseError<&'a str>>
where
    F: Parser<&'a str, O1, VerboseError<&'a str>>,
{
    preceded(terminated(tag(kw), multispace1), inner)
}

fn integer(i: &str) -> IResult<&str, Literal, VerboseError<&str>> {
    map_res(digit1, |int: &str| int.parse::<i64>().map(Literal::Integer))(i)
}

fn boolean(i: &str) -> IResult<&str, Literal, VerboseError<&str>> {
    map(
        alt((map(tag("true"), |_| true), map(tag("false"), |_| false))),
        Literal::Boolean,
    )(i)
}

// No escaped characters, classic
fn string(i: &str) -> IResult<&str, Literal, VerboseError<&str>> {
    map(
        delimited(
            char('"'),
            verify(is_not("\""), |s: &str| !s.is_empty()),
            char('"'),
        ),
        Literal::String,
    )(i)
}

fn identifier(i: &str) -> IResult<&str, Literal, VerboseError<&str>> {
    map(ident, Literal::Identifier)(i)
}

fn ident(i: &str) -> IResult<&str, &str, VerboseError<&str>> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(i)
}

fn if_expr(i: &str) -> IResult<&str, Literal, VerboseError<&str>> {
    let (i, cond) = keyword0("if", parens(expr))(i)?;
    let (i, consequence) = spaced(squirly(block))(i)?;
    let (i, alternative) = opt(spaced(keyword0("else", squirly(block))))(i)?;
    Ok((
        i,
        Literal::If {
            condition: Box::new(cond),
            consequence,
            alternative,
        },
    ))
}

fn func(i: &str) -> IResult<&str, Literal, VerboseError<&str>> {
    let (i, args) = keyword0("fn", parens(separated_list0(char(','), spaced(ident))))(i)?;
    let (i, body) = spaced(squirly(block))(i)?;
    Ok((i, Literal::Function { args, body }))
}

fn literal(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    spaced(alt((
        map(
            alt((if_expr, func, integer, boolean, string, identifier)),
            Expression::Literal,
        ),
        parens(expr),
    )))(i)
}

fn index(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (i, term) = literal(i)?;
    let (i, args) = many0(spaced(squarely(map(expr, Operator::Index))))(i)?;
    Ok((i, fold_exprs(term, args)))
}

fn call(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (i, term) = index(i)?;
    let (i, args) = many0(spaced(parens(|i| {
        let (i, args) = separated_list0(char(','), expr)(i)?;
        Ok((i, Operator::Call(args)))
    })))(i)?;
    Ok((i, fold_exprs(term, args)))
}

fn prefix(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (i, mut prefixes) = many0(alt((
        |i| {
            let i = char('-')(i)?.0;
            Ok((i, (Operator::Unary(ast::Unary::Neg))))
        },
        |i| {
            let i = char('!')(i)?.0;
            Ok((i, (Operator::Unary(ast::Unary::Not))))
        },
    )))(i)?;
    let (i, term) = call(i)?;
    prefixes.reverse();
    Ok((i, fold_exprs(term, prefixes)))
}

fn mul_div(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (i, initial) = prefix(i)?;
    let (i, remainder) = many0(alt((
        |i| {
            let (i, mul) = preceded(char('*'), prefix)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::Mul, mul))))
        },
        |i| {
            let (i, div) = preceded(char('/'), prefix)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::Div, div))))
        },
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

fn sum(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (i, initial) = mul_div(i)?;
    let (i, remainder) = many0(alt((
        |i| {
            let (i, add) = preceded(char('+'), mul_div)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::Add, add))))
        },
        |i| {
            let (i, sub) = preceded(char('-'), mul_div)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::Sub, sub))))
        },
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

fn lt_gt(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (i, initial) = sum(i)?;
    let (i, remainder) = many0(alt((
        |i| {
            let (i, lt) = preceded(char('<'), sum)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::LT, lt))))
        },
        |i| {
            let (i, gt) = preceded(char('>'), sum)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::GT, gt))))
        },
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

fn fold_exprs<'a>(initial: Expression<'a>, remainder: Vec<Operator<'a>>) -> Expression<'a> {
    remainder.into_iter().fold(initial, |acc, oper| match oper {
        Operator::Unary(u) => Expression::Prefix(u, Box::new(acc)),
        Operator::Binary(b, expr) => Expression::Infix(Box::new(acc), b, Box::new(expr)),
        Operator::Call(args) => Expression::Call {
            function: Box::new(acc),
            args,
        },
        Operator::Index(expr) => Expression::Index {
            left: Box::new(acc),
            index: Box::new(expr),
        },
    })
}

// Parens -> Expr
// Literal | Parens
// Index
// Call
// Prefix
// Product
// Sum
// LTGT
// EqNotEq
fn expr(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (i, initial) = lt_gt(i)?;
    let (i, remainder) = many0(alt((
        |i| {
            let (i, eq) = preceded(tag("=="), lt_gt)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::Eq, eq))))
        },
        |i| {
            let (i, neq) = preceded(tag("!="), lt_gt)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::Neq, neq))))
        },
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

fn expr_statement(i: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    map(terminated(expr, opt(char(';'))), Statement::Expression)(i)
}

fn let_statement(i: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    map(
        terminated(
            keyword1("let", separated_pair(ident, spaced(char('=')), expr)),
            preceded(multispace0, char(';')),
        ),
        |(ident, expr)| Statement::Let(ident, expr),
    )(i)
}

fn return_statement(i: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    map(
        terminated(keyword1("return", expr), preceded(multispace0, char(';'))),
        Statement::Return,
    )(i)
}

fn statement(i: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    spaced(alt((let_statement, return_statement, expr_statement)))(i)
}

fn block(i: &str) -> IResult<&str, BlockStatement, VerboseError<&str>> {
    map(many0(statement), BlockStatement)(i)
}

pub fn program(i: &str) -> IResult<&str, Program, VerboseError<&str>> {
    map(block, Program)(i)
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        self, Binary,
        Expression::{Call, Index, Infix, Literal, Prefix},
        Literal::{Boolean, Function, Identifier, If, Integer, String},
        Statement::{self, Expression, Let, Return},
    };

    macro_rules! assert_matches {
        ($statement: expr, $val: pat $(if $guard:expr)? $(,)?) => {
            assert!(matches!($statement, $val $(if $guard)?))
        };
    }

    macro_rules! assert_literal {
        ($statement: expr, $val: pat $(if $guard:expr)? $(,)?) => {
            assert_matches!($statement, Expression(Literal($val)))
        };
    }

    macro_rules! assert_prefix {
        ($statement: expr, $operator: pat, $right: pat) => {
            assert_matches!($statement, Expression(Prefix($operator, h)) if matches!(h.as_ref(), Literal($right)));
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
            if let Expression(Call { function, args }) = $statement {
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
            if let Expression(Literal(Function { args, body })) = $statement {
                assert_eq!(args.len(), $argc);
                assert_eq!(body.0.len(), $len);
                $asserts(args, body);
            } else {
                panic!("not a function");
            }
        };
    }

    fn parse_program(input: &str) -> Vec<Statement> {
        super::program(input).expect("must parse").1 .0 .0
    }

    #[test]
    fn test_literals() {
        let program = parse_program(
            r#"
            5;
            true;
            false;
            foo;
            "hello world";
        "#,
        );
        assert_eq!(program.len(), 5);
        assert_literal!(&program[0], Integer(5));
        assert_literal!(&program[1], Boolean(true));
        assert_literal!(&program[2], Boolean(false));
        assert_literal!(&program[3], Identifier("foo"));
        assert_literal!(&program[4], String("hello world"));
    }

    #[test]
    fn test_prefix_expressions() {
        let program = parse_program(
            r#"
            !5;
            -15;
            !true;
            !false;
        "#,
        );

        assert_eq!(program.len(), 4);
        use crate::ast::Unary::{Neg, Not};
        assert_prefix!(&program[0], Not, Integer(5));
        assert_prefix!(&program[1], Neg, Integer(15));
        assert_prefix!(&program[2], Not, Boolean(true));
        assert_prefix!(&program[3], Not, Boolean(false));
    }

    #[test]
    fn test_call_expression() {
        let program = parse_program(
            r#"
            add(1, 2 * 3, 4 + 5);
        "#,
        );

        assert_eq!(program.len(), 1);
        use crate::ast::Binary::{Add, Mul};
        assert_call!(&program[0], "add", 3, |args: &Vec<ast::Expression>| {
            assert_matches!(&args[0], Literal(Integer(1)));
            assert_infix_expr!(&args[1], Integer(2), Mul, Integer(3));
            assert_infix_expr!(&args[2], Integer(4), Add, Integer(5));
        });
    }

    #[test]
    fn test_call_parameter_parsing() {
        let program = parse_program(
            r#"
            add();
            add(1);
            mult(1, 2 * 3, 4 + 5);
        "#,
        );

        assert_eq!(program.len(), 3);
        use crate::ast::Binary::{Add, Mul};
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
        let program = parse_program(
            r#"
            myArray[1 + 1];
        "#,
        );

        assert_eq!(program.len(), 1);
        use crate::ast::Binary::Add;
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
        );

        assert_eq!(program.len(), 11);
        use crate::ast::Binary::{Add, Div, Eq, Mul, Neq, Sub, GT, LT};
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
        let program = parse_program(
            r#"
            let x = 5;
            let y = true;
            let foobar = y;
        "#,
        );

        assert_eq!(program.len(), 3);
        assert_matches!(&program[0], Let("x", Literal(Integer(5))));
        assert_matches!(&program[1], Let("y", Literal(Boolean(true))));
        assert_matches!(&program[2], Let("foobar", Literal(Identifier("y"))));
    }

    #[test]
    fn test_return_statement() {
        let program = parse_program(
            r#"
            return 5;
            return true;
            return foobar;
        "#,
        );

        assert_eq!(program.len(), 3);
        assert_matches!(&program[0], Return(Literal(Integer(5))));
        assert_matches!(&program[1], Return(Literal(Boolean(true))));
        assert_matches!(&program[2], Return(Literal(Identifier("foobar"))));
    }

    #[test]
    fn test_if_expression() {
        let program = parse_program(
            r#"
            if (x < y) { x };
            if (x < y) { x } else { y };
        "#,
        );

        assert_eq!(program.len(), 2);
        use Binary::LT;
        assert_matches!(
            &program[0],
            Expression(Literal(If {
                condition,
                consequence,
                alternative: None
            })) if {
                matches!(consequence.0.len(), 1) &&
                infix_expr_matches!(condition.as_ref(), Identifier("x"), LT, Identifier("y")) &&
                matches!(&consequence.0[0], Expression(Literal(Identifier("x"))))
            }
        );
        assert_matches!(
            &program[1],
            Expression(Literal(If {
                condition,
                consequence,
                alternative: Some(alt),
            })) if {
                matches!(consequence.0.len(), 1) &&
                infix_expr_matches!(condition.as_ref(), Identifier("x"), LT, Identifier("y")) &&
                matches!(&consequence.0[0], Expression(Literal(Identifier("x")))) &&
                matches!(&alt.0.len(), 1) &&
                matches!(&alt.0[0], Expression(Literal(Identifier("y"))))
            }
        );
    }

    #[test]
    fn test_function_literal() {
        let program = parse_program(
            r#"
            fn(x, y) { x + y; };
        "#,
        );

        assert_eq!(program.len(), 1);
        use Binary::Add;
        assert_fn!(
            &program[0],
            2,
            1,
            |args: &Vec<&str>, body: &ast::BlockStatement| {
                assert_eq!(args[0], "x");
                assert_eq!(args[1], "y");
                assert_infix!(&body.0[0], Identifier("x"), Add, Identifier("y"));
            }
        );
    }

    #[test]
    fn test_function_parameter_parsing() {
        let program = parse_program(
            r#"
            fn(){};
            fn(x){};
            fn(x,y,z){};
        "#,
        );

        assert_eq!(program.len(), 3);
        assert_fn!(&program[0], 0, 0, |_, _| {});
        assert_fn!(&program[1], 1, 0, |args: &Vec<&str>, _| {
            assert_eq!(args[0], "x");
        });
        assert_fn!(&program[2], 3, 0, |args: &Vec<&str>, _| {
            assert_eq!(args[0], "x");
            assert_eq!(args[1], "y");
            assert_eq!(args[2], "z");
        });
    }

    #[test]
    fn test_operator_precedence() {
        let cases = vec![
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            ("a + add[b * c] + d", "((a + (add[(b * c)])) + d)"),
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
        ];

        for (case, want) in cases {
            let program = parse_program(case);
            assert_eq!(program.len(), 1);
            assert_matches!(&program[0], Expression(expr) if {
                let prog = expr.to_string();
                if prog != want {
                    println!("case: {case}");
                    println!("got: {prog}");
                    println!("want: {want}");
                }
                prog == want
            });
        }
    }
}
