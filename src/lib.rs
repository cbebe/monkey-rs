mod ast;

use ast::{Expression, Program, Statement};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, multispace1, one_of},
    combinator::{cut, map, opt, recognize},
    error::VerboseError,
    multi::{many0, many0_count, many1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

fn parse_integer(input: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    map(digit1, |i: &str| {
        Expression::Integer(i.parse::<i64>().unwrap())
    })(input)
}

fn parse_boolean(input: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    map(
        alt((map(tag("true"), |_| true), map(tag("false"), |_| false))),
        Expression::Boolean,
    )(input)
}

fn parse_identifier(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn parse_expression(input: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    alt((
        parse_integer,
        parse_boolean,
        map(parse_identifier, |i: &str| Expression::Identifier(i)),
    ))(input)
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
                parse_identifier,
                delimited(multispace0, tag("="), multispace0),
                parse_expression,
            ),
            tag(";"),
        ),
        |(ident, expr)| Statement::Let(ident.to_string(), expr),
    )(input)
}

fn parse_statement(input: &str) -> IResult<&str, Statement, VerboseError<&str>> {
    delimited(
        multispace0,
        alt((parse_let_statement, parse_expression_statement)),
        multispace0,
    )(input)
}

pub fn parse_program(input: &str) -> IResult<&str, Program, VerboseError<&str>> {
    map(many0(parse_statement), Program)(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expression, Statement};

    macro_rules! assert_let {
        ($statement: expr, $ident: expr, $val: pat) => {
            if let Statement::Let(f, $val) = $statement {
                if f != $ident {
                    panic!()
                }
            } else {
                panic!()
            }
        };
    }

    #[test]
    fn test_literals() {
        let (_, program) = parse_program(
            r#"
            5;
            true;
            false;
            foo;
        "#,
        )
        .expect("must parse");
        assert_eq!(program.0.len(), 4);
        assert!(matches!(
            &program.0[0],
            Statement::Expression(Expression::Integer(5)),
        ));
        assert!(matches!(
            &program.0[1],
            Statement::Expression(Expression::Boolean(true)),
        ));
        assert!(matches!(
            &program.0[2],
            Statement::Expression(Expression::Boolean(false)),
        ));
        assert!(matches!(
            &program.0[3],
            Statement::Expression(Expression::Identifier("foo")),
        ));
    }

    #[test]
    fn test_let_statement() {
        let (_, program) = parse_program(
            r#"
            let x = 5;
            let y = true;
            let foobar = y;
        "#,
        )
        .expect("must parse");

        assert_eq!(program.0.len(), 3);
        assert_let!(&program.0[0], "x", Expression::Integer(5));
        assert_let!(&program.0[1], "y", Expression::Boolean(true));
        assert_let!(&program.0[2], "foobar", Expression::Identifier("y"));
    }
}
