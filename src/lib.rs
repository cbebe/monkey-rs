mod ast;
mod expr;

use ast::{Expression, Operator, Program, Statement};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, multispace1, one_of},
    combinator::{map, opt, recognize, verify},
    error::VerboseError,
    multi::{many0, many0_count},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
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

// No escaped characters, classic
fn parse_string(input: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    map(
        delimited(
            char('"'),
            verify(is_not("\""), |s: &str| !s.is_empty()),
            char('"'),
        ),
        |i| Expression::String(i),
    )(input)
}

fn parse_prefix(input: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    map(
        separated_pair(one_of("-!"), multispace0, parse_expression),
        |(oper, expr)| {
            Expression::Prefix(
                match oper {
                    '-' => Operator::Minus,
                    '!' => Operator::Bang,
                    _ => panic!(),
                },
                Box::new(expr),
            )
        },
    )(input)
}

fn identifier(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn parse_identifier(input: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    map(identifier, |i| Expression::Identifier(i))(input)
}

fn parse_expression(input: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    delimited(
        multispace0,
        alt((
            parse_prefix,
            parse_string,
            parse_integer,
            parse_boolean,
            parse_identifier,
        )),
        multispace0,
    )(input)
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
        Expression::{Boolean, Identifier, Infix, Integer, Prefix, String},
        Operator,
        Statement::{self, Expression, Let, Return},
    };

    macro_rules! assert_matches {
        ($statement: expr, $val: pat $(if $guard:expr)? $(,)?) => {
            assert!(matches!($statement, $val $(if $guard)?))
        };
    }

    macro_rules! assert_prefix {
        ($statement: expr, $operator: pat, $right: pat) => {
            assert_matches!($statement, Expression(Prefix($operator, h)) if matches!(h.as_ref(), $right));
        };
    }

    macro_rules! assert_infix {
        ($statement: expr, $left: pat, $operator: pat, $right: pat) => {
            assert_matches!($statement, Expression(Infix(left, $operator, right)) if matches!(left.as_ref(), $left) && matches!(right.as_ref(), $right));
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
        assert_matches!(&program[0], Expression(Integer(5)));
        assert_matches!(&program[1], Expression(Boolean(true)));
        assert_matches!(&program[2], Expression(Boolean(false)));
        assert_matches!(&program[3], Expression(Identifier("foo")));
        assert_matches!(&program[4], Expression(String("hello world")));
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
        use Operator::{Bang, Minus};
        assert_prefix!(&program[0], Bang, Integer(5));
        assert_prefix!(&program[1], Minus, Integer(15));
        assert_prefix!(&program[2], Bang, Boolean(true));
        assert_prefix!(&program[3], Bang, Boolean(false));
    }

    // #[test]
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

        assert_eq!(program.len(), 10);
        use Operator::{Asterisk, Eq, Minus, NotEq, Plus, Slash, GT, LT};
        assert_infix!(&program[0], Integer(3), Plus, Integer(10));
        assert_infix!(&program[1], Integer(5), Minus, Integer(5));
        assert_infix!(&program[2], Integer(5), Asterisk, Integer(5));
        assert_infix!(&program[3], Integer(5), Slash, Integer(5));
        assert_infix!(&program[4], Integer(5), GT, Integer(5));
        assert_infix!(&program[5], Integer(5), LT, Integer(5));
        assert_infix!(&program[6], Integer(5), Eq, Integer(5));
        assert_infix!(&program[7], Integer(5), NotEq, Integer(5));
        assert_infix!(&program[8], Boolean(true), Eq, Boolean(true));
        assert_infix!(&program[9], Boolean(false), Eq, Boolean(false));
        assert_infix!(&program[10], Boolean(true), NotEq, Boolean(false));
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
        assert_matches!(&program[0], Let("x", Integer(5)));
        assert_matches!(&program[1], Let("y", Boolean(true)));
        assert_matches!(&program[2], Let("foobar", Identifier("y")));
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
        assert_matches!(&program[0], Return(Integer(5)));
        assert_matches!(&program[1], Return(Boolean(true)));
        assert_matches!(&program[2], Return(Identifier("foobar")));
    }
}
