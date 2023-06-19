mod ast;

use ast::{Binary, Expression, Literal, Operator, Program, Statement};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, multispace1, one_of},
    combinator::{map, map_res, opt, recognize, verify},
    error::VerboseError,
    multi::{many0, many0_count},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    IResult,
};

fn parse_integer(input: &str) -> IResult<&str, Literal, VerboseError<&str>> {
    map_res(digit1, |int: &str| int.parse::<i64>().map(Literal::Integer))(input)
}

fn parse_boolean(input: &str) -> IResult<&str, Literal, VerboseError<&str>> {
    map(
        alt((map(tag("true"), |_| true), map(tag("false"), |_| false))),
        Literal::Boolean,
    )(input)
}

// No escaped characters, classic
fn parse_string(input: &str) -> IResult<&str, Literal, VerboseError<&str>> {
    map(
        delimited(
            char('"'),
            verify(is_not("\""), |s: &str| !s.is_empty()),
            char('"'),
        ),
        Literal::String,
    )(input)
}

fn parse_identifier(input: &str) -> IResult<&str, Literal, VerboseError<&str>> {
    map(identifier, Literal::Identifier)(input)
}

fn identifier(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn parens(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    delimited(
        multispace0,
        delimited(tag("("), parse_expression, tag(")")),
        multispace0,
    )(i)
}

fn literal(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    delimited(
        multispace0,
        alt((
            map(
                alt((parse_integer, parse_boolean, parse_string, parse_identifier)),
                Expression::Literal,
            ),
            parens,
        )),
        multispace0,
    )(i)
}

fn index(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    todo!("parse the initial, then the index")
}

fn call(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    todo!("parse the initial, then the arguments")
}

fn prefix(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (i, prefixes) = many0(alt((
        |i| {
            let i = tag("-")(i)?.0;
            Ok((i, (Operator::Unary(ast::Unary::Neg))))
        },
        |i| {
            let i = tag("!")(i)?.0;
            Ok((i, (Operator::Unary(ast::Unary::Not))))
        },
    )))(i)?;
    // TODO: Parse call expressions
    // let (i, term) = call(i)?;
    let (i, term) = literal(i)?;
    Ok((i, fold_exprs(term, prefixes)))
}

fn mul_div(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (i, initial) = prefix(i)?;
    let (i, remainder) = many0(alt((
        |i| {
            let (i, mul) = preceded(tag("*"), prefix)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::Mul, mul))))
        },
        |i| {
            let (i, div) = preceded(tag("/"), prefix)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::Div, div))))
        },
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

fn sum(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (i, initial) = mul_div(i)?;
    let (i, remainder) = many0(alt((
        |i| {
            let (i, add) = preceded(tag("+"), mul_div)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::Add, add))))
        },
        |i| {
            let (i, sub) = preceded(tag("-"), mul_div)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::Sub, sub))))
        },
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

fn lt_gt(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (i, initial) = sum(i)?;
    let (i, remainder) = many0(alt((
        |i| {
            let (i, lt) = preceded(tag("<"), sum)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::LT, lt))))
        },
        |i| {
            let (i, gt) = preceded(tag(">"), sum)(i)?;
            Ok((i, (Operator::Binary(ast::Binary::GT, gt))))
        },
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

fn fold_exprs<'a>(initial: Expression<'a>, remainder: Vec<Operator<'a>>) -> Expression<'a> {
    remainder.into_iter().fold(initial, |acc, oper| match oper {
        Operator::Unary(u) => Expression::Prefix(u, Box::new(acc)),
        Operator::Binary(b, expr) => Expression::Infix(Box::new(acc), b, Box::new(expr)),
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
fn parse_expression(i: &str) -> IResult<&str, Expression, VerboseError<&str>> {
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

fn parse_expression_statement(input: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    map(
        terminated(parse_expression, opt(tag(";"))),
        Statement::Expression,
    )(input)
}

fn parse_let_statement(input: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    map(
        delimited(
            terminated(tag("let"), multispace1),
            separated_pair(
                identifier,
                delimited(multispace0, tag("="), multispace0),
                parse_expression,
            ),
            tag(";"),
        ),
        |(ident, expr)| Statement::Let(ident, expr),
    )(input)
}

fn parse_return_statement(input: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    map(
        delimited(
            terminated(tag("return"), multispace1),
            parse_expression,
            preceded(multispace0, tag(";")),
        ),
        Statement::Return,
    )(input)
}

fn parse_statement(input: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    delimited(
        multispace0,
        alt((
            parse_let_statement,
            parse_return_statement,
            parse_expression_statement,
        )),
        multispace0,
    )(input)
}

pub fn parse_program(input: &str) -> IResult<&str, Program, VerboseError<&str>> {
    map(many0(parse_statement), Program)(input)
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        Expression::{Infix, Literal, Prefix},
        Literal::{Boolean, Identifier, Integer, String},
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

    macro_rules! assert_infix {
        ($statement: expr, $left: pat, $operator: pat, $right: pat) => {
            assert_matches!($statement, Expression(Infix(left, $operator, right)) if matches!(left.as_ref(), Literal($left)) && matches!(right.as_ref(), Literal($right)));
        };
    }

    fn parse_program(input: &str) -> Vec<Statement> {
        super::parse_program(input).expect("must parse").1 .0
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

        dbg!(&program);
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
}
