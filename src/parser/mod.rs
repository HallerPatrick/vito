mod statements;

use std::cell::RefCell;
use crate::lexer::Token;
use crate::parser::statements::{Expression, Identifier, LHAssignment, Literal, UnaryOperator, BinaryOperator};

use thiserror::Error;

#[derive(Error, Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxError<'a> {
    #[error("Syntax error in assignment: {0}")]
    Assignment(&'a str),
    #[error("Syntax error in function definition: {0}")]
    FunctionDef(&'a str),
    #[error("Syntax error in function call: {0}")]
    FunctionCall(&'a str),
    #[error("Syntax error in if block: {0}")]
    IfBlock(&'a str),
    #[error("Syntax error in for block: {0}")]
    ForBlock(&'a str),
    #[error("Syntax error in expression: {0}")]
    Expression(&'a str)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Assignment(LHAssignment),
    FunctionDef,
    FunctionCall(Identifier, Vec<Expression>),
    IfBlock,
    ForBlock,
}

/// Blocks are a list of statements that may have a return value
/// They are used to represent functions, if blocks, for blocks, etc.
#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub return_value: Option<Expression>,
}

impl Block {
    pub fn new() -> Block {
        Block {
            statements: Vec::new(),
            return_value: None,
        }
    }
}
type TokenIterator<'a> = std::iter::Peekable<std::slice::Iter<'a, Token>>;

/// Parser builds the AST for our language
///
/// In `Vito` everything can be parsed into
/// a list of statements
pub struct Parser <'a>{
    tokens: Vec<Token>,

    // Use a consumable iterator
    // We having major problems using the token iter as a mutuable reference during parsing
    // With RefCell we can get around this, because we dont have to get a mutuable ref of self
    token_iter: RefCell<TokenIterator<'a>>,
}

macro_rules! expect_token_with_value {
    ($enum_val:expr, $enum_type:ident::$variant_name:ident, $err_enum:ident::$err_variant:ident, $err_msg:literal) => {
        match $enum_val {
            $enum_type::$variant_name(_) => true,
            _ => return Err($err_enum::$err_variant($err_msg)),
        }
    };
}

macro_rules! expect_token {
    ($enum_val:expr, $enum_type:ident::$variant_name:ident, $err_enum:ident::$err_variant:ident, $err_msg:literal) => {
        match $enum_val {
            $enum_type::$variant_name => true,
            _ => return Err($err_enum::$err_variant($err_msg)),
        }
    };
}

impl <'a>Parser<'a> {
    pub fn new(tokens: &Vec<Token>) -> Parser {
        Parser {
            tokens: tokens.clone(),
            token_iter: RefCell::new(tokens.iter().peekable()),
        }
    }

    pub fn parse(&mut self) -> Result<Block, SyntaxError> {
        // let mut token_iter = self.tokens.iter().peekable();
        self.parse_block()
    }

    fn peek(&self) -> Option<&Token> {
        self.token_iter.borrow_mut().peek().map(|t| *t)
    }

    fn next(&self) -> Option<&Token> {
        self.token_iter.borrow_mut().next()
    }


    fn parse_block(&mut self) -> Result<Block, SyntaxError> {
        let mut block = Block::new();

        match self.parse_statements() {
            Ok(statements) => block.statements = statements,
            Err(err) => return Err(err),
        }
        
        println!("Statements: {:?}", block.statements);
        if let Some(token) = self.peek() {
            println!("Token: {:?}", token);
            match token {
                Token::Return => {
                    self.next();
                    match self.parse_expression() {
                        Ok(expr) => block.return_value = Some(expr),
                        Err(err) => return Err(err),
                    }
                }
                Token::EOF => {}
                _ => {
                    panic!("Unexpected token: {:?}", token)
                }
            }
        }

        Ok(block)
    }

    fn parse_statements(&self) -> Result<Vec<Statement>, SyntaxError> {
        let mut statements = Vec::new();
        while let Some(token) = self.peek() {
            if !self.is_statement() {
                println!("Not a statement: {:?}", self.peek());
                break;
            }

            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(err) => return Err(err),
            }
        }

        Ok(statements)
    }

    fn is_statement(&self) -> bool {
        match self.peek() {
            Some(Token::Let) => true,
            Some(Token::Identifier(_)) => true,
            _ => false,
        }
    }

    fn parse_statement(&self) -> Result<Statement, SyntaxError> {
        // The first token should be a keyword, except for reassignment and function calls
        let token = self.peek().unwrap();

        match token {
            Token::Let => self.parse_assignment(),

            // TODO: For now, we only support function calls
            Token::Identifier(ident) => self.parse_func_call(),
            _ => todo!(),
        }
    }

    fn parse_assignment(&self) -> Result<Statement, SyntaxError> {
        // Let keyword
        expect_token!(self.next().unwrap(), Token::Let, SyntaxError::Assignment, "Expected 'let' keyword");

        // Identifier
        let identifier = self.next().unwrap().clone();
        expect_token_with_value!(identifier, Token::Identifier, SyntaxError::Assignment, "Expected identifier");

        // Equal sign
        expect_token!(self.next().unwrap(), Token::Equal, SyntaxError::Assignment, "Expected '='");

        // Right hand expression
        let expression = match self.parse_expression() {
            Ok(expr) => expr,
            Err(err) => return Err(err),
        };

        // Terminator
        expect_token!(
            self.next().unwrap(),
            Token::Terminator,
            SyntaxError::Assignment,
            "Expected ';'"
        );

        Ok(Statement::Assignment(LHAssignment {
            variable: Identifier::from(&identifier),
            expression,
        }))
    }

    /// Parses an expression
    /// An expression is a list of tokens that can be evaluated to a value
    /// For example, `1 + 2` is an expression that evaluates to `3`
    /// `1 + 2 * 3` is also an expression that evaluates to `7`
    /// 
    /// We are using recursive descent parsing to parse expressions
    fn parse_expression(&self) -> Result<Expression, SyntaxError>{

        // Parse the first term
        let mut expr = self.parse_term()?;
        println!("Term: {:?}", expr);

        // Parse the rest of the expression
        while let Some(token) = self.peek() {
            match token {
                Token::Plus => {
                    self.next();
                    let rhs = self.parse_term()?;
                    expr = Expression::BinaryOp(Box::new(expr), Box::new(rhs), BinaryOperator::Add);
                }
                Token::Minus => {
                    self.next();
                    let rhs = self.parse_term()?;
                    expr = Expression::BinaryOp(Box::new(expr), Box::new(rhs), BinaryOperator::Subtract);
                }
                _ => break,
            }
        }

        Ok(expr)

    }

    /// Parses a term
    /// A term is a list of tokens that can be evaluated to a value
    fn parse_term(&self) -> Result<Expression, SyntaxError>{

        // Parse the first factor
        let mut expr = self.parse_factor()?;

        // // Parse the rest of the expression
        while let Some(token) = self.peek() {
            match token {
                Token::Asterisk => {
                    self.next();
                    let rhs = self.parse_factor()?;
                    expr = Expression::BinaryOp(Box::new(expr), Box::new(rhs), BinaryOperator::Multiply);
                }
                Token::Slash => {
                    self.next();
                    let rhs = self.parse_factor()?;
                    expr = Expression::BinaryOp(Box::new(expr), Box::new(rhs), BinaryOperator::Divide);
                }
                _ => break,
            }
        }

        Ok(expr)

    }

    /// Parses a factor
    fn parse_factor(&self) -> Result<Expression, SyntaxError>{

        if let Some(token) = self.next() {
            println!("Token(factor): {:?}", token);
            match token {
                Token::IntegerLiteral(literal_value) => {
                    return Ok(Expression::Literal(Literal::Integer(*literal_value)))
                }
                Token::StringLiteral(literal_value) => {
                    return Ok(Expression::Literal(Literal::String(literal_value.clone())))
                }
                Token::Identifier(ident) => {
                    return Ok(Expression::Identifier(Identifier::from(&Token::Identifier(ident.clone()))))
                }
                Token::Minus => {
                    let rhs = self.parse_factor()?;
                    return Ok(Expression::UnaryOp(Box::new(rhs), UnaryOperator::Negate))
                }
                _ => return Err(SyntaxError::Expression("Unexpected token")),
            };
        }

        Err(SyntaxError::Expression("Unexpected end of expression"))
    }

    fn parse_func_call(&self) -> Result<Statement, SyntaxError> {

        // Identifier
        let identifier = self.next().unwrap().clone();
        expect_token_with_value!(identifier, Token::Identifier, SyntaxError::FunctionCall, "Expected identifier");

        expect_token!(
            self.next().unwrap(),
            Token::LeftParen,
            SyntaxError::FunctionCall,
            "Expected '('"
        );

        let mut arguments = Vec::new();

        // TODO: Check for orderin 1. expression, 2. comma 3. expression etc
        while let Some(token) = self.peek() {
            match token {
                Token::RightParen => {
                    self.next();
                    break;
                }
                Token::Comma => {
                    self.next();
                }
                _ => match self.parse_expression() {
                    Ok(expr) => arguments.push(expr),
                    _ => todo!(),
                    // Err(err) => return Err(err),
                },
            }
        }

        expect_token!(
            self.next().unwrap(),
            Token::Terminator,
            SyntaxError::FunctionCall,
            "Expected ';'"
        );

        Ok(Statement::FunctionCall(
            Identifier::from(&identifier),
            arguments,
        ))
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_assignment_integer() {
        let tokens = vec![
            Token::Let,
            Token::Identifier(String::from("x")),
            Token::Equal,
            Token::IntegerLiteral(5),
            Token::Terminator,
        ];

        let mut parser = Parser::new(&tokens);

        let ast = parser.parse().unwrap();

        assert_eq!(
            ast,
            Block {
                statements: vec![Statement::Assignment(LHAssignment {
                    variable: Identifier::from(&Token::Identifier(String::from("x"))),
                    expression: Expression::Literal(Literal::Integer(5)),
                })],
                return_value: None,
            }
        );
    }

    #[test]
    fn test_assignment_string() {
        let tokens = vec![
            Token::Let,
            Token::Identifier(String::from("x")),
            Token::Equal,
            Token::StringLiteral(String::from("Hello World!")),
            Token::Terminator,
        ];

        let mut parser = Parser::new(&tokens);

        let ast = parser.parse().unwrap();

        assert_eq!(
            ast,
            Block {
                statements: vec![Statement::Assignment(LHAssignment {
                    variable: Identifier::from(&Token::Identifier(String::from("x"))),
                    expression: Expression::Literal(Literal::String(String::from("Hello World!"))),
                })],
                return_value: None,
            }
        );
    }

    #[test]
    fn test_function_call() {
        let tokens = vec![
            Token::Identifier(String::from("print")),
            Token::LeftParen,
            Token::StringLiteral(String::from("Hello World!")),
            Token::RightParen,
            Token::Terminator,
        ];

        let mut parser = Parser::new(&tokens);

        let ast = parser.parse().unwrap();

        assert_eq!(
            ast,
            Block {
                statements: vec![Statement::FunctionCall(
                    Identifier::from(&Token::Identifier(String::from("print"))),
                    vec![Expression::Literal(Literal::String(String::from(
                        "Hello World!"
                    )))]
                )],
                return_value: None,
            }
        );
    }

    #[test]
    fn test_two_statemnts() {
        let tokens = vec![
            Token::Let,
            Token::Identifier("x".to_string()),
            Token::Equal,
            Token::IntegerLiteral(3),
            Token::Terminator,
            Token::Identifier("print".to_string()),
            Token::LeftParen,
            Token::Identifier("x".to_string()),
            Token::RightParen,
            Token::Terminator,
            Token::EOF,
        ];

        let mut parser = Parser::new(&tokens);

        let ast = parser.parse().unwrap();

        assert_eq!(
            ast,
            Block {
                statements: vec![
                    // Statment 1
                    Statement::Assignment(LHAssignment {
                    variable: Identifier::from(&Token::Identifier("x".to_string())),
                    expression: Expression::Literal(Literal::Integer(3))
                    }),
                    // Statement 2
                    Statement::FunctionCall(
                        Identifier::from(&Token::Identifier("print".to_string())),
                        vec![Expression::Identifier(Identifier{name: "x".to_string()})]
                    )

                ],
                return_value: None,
            }
        );
    }

    #[test]
    fn test_syntax_error_function_call() {
        let tokens = vec![
            Token::Identifier(String::from("print")),
            // Missing left paren
            // Token::LeftParen,
            Token::StringLiteral(String::from("Hello World!")),
            Token::RightParen,
            Token::Terminator,
        ];

        let mut parser = Parser::new(&tokens);

        let ast = parser.parse();

        assert!(ast.is_err());
    }
}

mod tests_expression {

    use super::*;

    
    #[test]
    fn test_binary_expression_add() {
        let tokens = vec![
            Token::Return,
            Token::IntegerLiteral(1),
            Token::Plus,
            Token::IntegerLiteral(2),
            Token::Terminator,
        ];

        let mut parser = Parser::new(&tokens);

        let ast = parser.parse();
        
        assert_eq!(
            ast,
            Ok(Block {
                statements: vec![],
                return_value: Some(Expression::BinaryOp(
                    Box::new(Expression::Literal(Literal::Integer(1))),
                    Box::new(Expression::Literal(Literal::Integer(2))),
                    BinaryOperator::Add,
                )),
            })
        );
    }

    #[test]
    fn test_binary_expression_multiply() {

        let tokens = vec![
            Token::Return,
            Token::IntegerLiteral(1),
            Token::Asterisk,
            Token::IntegerLiteral(2),
            Token::Terminator,
        ];

        let mut parser = Parser::new(&tokens);

        let ast = parser.parse();

        assert_eq!(
            ast,
            Ok(Block {
                statements: vec![],
                return_value: Some(Expression::BinaryOp(
                    Box::new(Expression::Literal(Literal::Integer(1))),
                    Box::new(Expression::Literal(Literal::Integer(2))),
                    BinaryOperator::Multiply,
                )),
            })
        );

    }

}
