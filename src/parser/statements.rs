use crate::lexer::Token;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Identifier {
    name: String
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(i32),
    String(String)
}

///Identifier can be casted from String
impl From<String> for Identifier {
    fn from(name: String) -> Self {
        Identifier {
            name
        }
    }
}

impl From<&Token> for Identifier {
    
    fn from(token: &Token) -> Self {
        match token {
            Token::Identifier(name) => Identifier::from(name.clone()),
            _ => panic!("Cannot convert token to identifier")
        }
    }

}


/// Interface that should be implemented for most statements, to be instantiated from tokens
trait FromToken {
    fn from_token(token: Token) -> Self;

}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Literal(Literal),
    Identifier(Identifier),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LHAssignment {
    pub variable: Identifier,
    pub expression: Expression
}

