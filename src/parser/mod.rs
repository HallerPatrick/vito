mod statements;

use crate::lexer::Token;
use crate::parser::statements::{ Expression, Literal, LHAssignment, Identifier };

#[derive(Debug, PartialEq, Eq)]
enum Statement {
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
    statements: Vec<Statement>,
    return_value: Option<Expression>,
}

impl Block {
    pub fn new() -> Block {
        Block {
            statements: Vec::new(),
            return_value: None,
        }
    }
}

/// Parser builds the AST for our language
///
/// In `Vito` everything can be parsed into
/// a list of statements
pub struct Parser<'a> {
    tokens: Vec<Token>,

    // Use a consumable iterator
    token_iter: std::iter::Peekable<std::slice::Iter<'a, Token>>,
}

macro_rules! expect_token_with_value {
    ($enum_val:expr, $enum_type:ident::$variant_name:ident) => {
        match $enum_val {
            Token::$variant_name(_) => true,
            _ => false,
        }
    }
}

macro_rules! expect_token {
    ($enum_val:expr, $enum_type:ident::$variant_name:ident) => {
        match $enum_val {
            Token::$variant_name => true,
            _ => false,
        }
    }
}


impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Parser<'a> {
        Parser {
            tokens: tokens.clone(),
            token_iter: tokens.iter().peekable(),
        }
    }

    fn next(&mut self) -> Option<&Token> {
        self.token_iter.next()
    }


    pub fn parse(&mut self) -> Block {
        let mut block = Block::new();
        
        self.parse_block()
    }

    fn parse_block(&mut self) -> Block {
        let mut block = Block::new();

        block.statements = self.parse_statements();

        if let Some(token) = self.token_iter.peek() {
            match token {
                Token::Return => {
                    self.token_iter.next();
                    block.return_value = Some(self.parse_expression());
                }
                Token::EOF => {},
                _ => unreachable!()
            }
        }

        block
    }

    fn parse_statements(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        while let Some(token) = self.token_iter.peek() {
            if !self.is_statement() {
                break;
            }

            statements.push(self.parse_statement());
        }

        // statements.push(self.parse_assignment());
        statements
    }

    fn is_statement(&mut self) -> bool {
        match self.token_iter.peek() {
            Some(Token::Let) => true,
            _ => false,
        }
    }

    fn parse_statement(&mut self) -> Statement {
        
        // The first token should be a keyword, except for reassignment and function calls
        let token = self.token_iter.peek().unwrap();
        
        println!("Statement Token: {:?}", token);
        match token {
            Token::Let => {
                self.parse_assignment()
            },

            // TODO: For now, we only support function calls
            Token::Identifier(ident) => {
                self.parse_func_call()
            }
            _ => todo!()
        }
    }

    fn parse_assignment(&mut self) -> Statement {
        // Let keyword
        expect_token!(self.next().unwrap(), Token::Let);

        // Identifier
        let identifier = self.next().unwrap().clone();
        expect_token_with_value!(identifier, Token::Identifier);

        // Equal sign
        expect_token!(self.next().unwrap(), Token::Equal);

        // Right hand expression
        let expression = self.parse_expression();

        // Terminator
        expect_token!(self.next().unwrap(), Token::Terminator);

        Statement::Assignment(LHAssignment {
            variable: Identifier::from(&identifier),
            expression,
        })

    }

    fn parse_expression(&mut self) -> Expression {
        let literal = self.next().unwrap();
        println!("Literal: {:?}", literal);
        match literal {
            Token::IntegerLiteral(literal_value) => Expression::Literal(Literal::Integer(*literal_value)),
            Token::StringLiteral(literal_value) => Expression::Literal(Literal::String(literal_value.clone())),
            Token::Identifier(ident) => Expression::Identifier(Identifier::from(&Token::Identifier(ident.clone()))),
            _ => todo!()
        }
    }

    fn parse_func_call(&mut self) -> Statement {
        // Identifier
        let identifier = self.next().unwrap().clone();
        expect_token_with_value!(identifier, Token::Identifier);

        expect_token!(self.next().unwrap(), Token::LeftParen);

        let mut arguments = Vec::new();
        
        println!("Token iter: {:?}", self.token_iter.peek());
        while let Some(token) = self.token_iter.peek() {
            match token {
                Token::RightParen => {
                    self.token_iter.next();
                    break;
                }
                Token::Comma => {
                    self.token_iter.next();
                }
                _ => arguments.push(self.parse_expression())
            }
        }

        expect_token!(self.next().unwrap(), Token::Terminator);
        
        Statement::FunctionCall(Identifier::from(&identifier), arguments)
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

        let ast = parser.parse();

        assert_eq!(ast, Block {
            statements: vec![
                Statement::Assignment(LHAssignment {
                    variable: Identifier::from(&Token::Identifier(String::from("x"))),
                    expression: Expression::Literal(Literal::Integer(5)),
                })
            ],
            return_value: None,
        });
    
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

        let ast = parser.parse();

        assert_eq!(ast, Block {
            statements: vec![
                Statement::Assignment(LHAssignment {
                    variable: Identifier::from(&Token::Identifier(String::from("x"))),
                    expression: Expression::Literal(Literal::String(String::from("Hello World!"))),
                })
            ],
            return_value: None,
        });
    
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

        let ast = parser.parse();

        assert_eq!(ast, Block {
            statements: vec![
                Statement::FunctionCall(
                    Identifier::from(&Token::Identifier(String::from("print"))),
                    vec![
                        Expression::Literal(Literal::String(String::from("Hello World!")))
                    ]
                )
            ],
            return_value: None,
        });
    
    }


}
