
#[derive(Debug, PartialEq)]
pub enum Token {
    // Keywords
    Let,
    // Operators
    Plus,
    Minus,
    Equal,
    Comma,
    // Identifiers
    Identifier(String),
    // Literals
    IntegerLiteral(i32),
    FloatLiteral(f32),
    StringLiteral(String),
    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Terminator,
    // Misc
    EOF,
}

#[derive(Debug)]
pub struct Lexer {
    input: String,
    read_position: usize,

    // For error reporting
    current_line: usize,
    current_column: usize,
}

impl Lexer {

    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input: input,
            read_position: 0,
            current_line: 0,
            current_column: 0,
        };
        l
    }

    fn current_char(&self) -> Option<char> {
        self.input.chars().nth(self.read_position)
    }

    pub fn run(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(c) = self.current_char() {
            match c {
                ' ' | '\n' => {
                    self.advance();
                },
                ';' => {
                    tokens.push(Token::Terminator);
                    self.advance();
                },
                '+' => {
                    tokens.push(Token::Plus);
                    self.advance();
                },
                '-' => {
                    tokens.push(Token::Minus);
                    self.advance();
                },
                '(' => {
                    tokens.push(Token::LeftParen);
                    self.advance();
                },
                ')' => {
                    tokens.push(Token::RightParen);
                    self.advance();
                },
                '{' => {
                    tokens.push(Token::LeftBrace);
                    self.advance();
                },
                '}' => {
                    tokens.push(Token::RightBrace);
                    self.advance();
                },
                '=' => {
                    tokens.push(Token::Equal);
                    self.advance();
                },
                ',' => {
                    tokens.push(Token::Comma);
                    self.advance();
                },
                // Match a number
                '0'..='9' => {
                    let number = self.read_number();
                    tokens.push(number);
                },
                // Match a string
                '"' => {
                    let string = self.read_string();
                    tokens.push(Token::StringLiteral(string));
                },
                // Match a keyword
                _ if c.is_alphabetic() => {
                    let ident = self.read_identifier();
                    match ident.as_str() {
                        "let" => {
                            tokens.push(Token::Let);
                        },
                        _ => {
                            tokens.push(Token::Identifier(ident));
                        },
                    }
                },
                _ => {
                    self.advance();
                },
            }
        }

        tokens
    }

    fn advance(&mut self) -> Option<char> {
        if self.read_position >= self.input.len() {
            return None;
        }
        let current_char = self.input.chars().nth(self.read_position).expect("Failed to get character at position");
        self.read_position += 1;
        if current_char == '\n' {
            self.current_line += 1;
            self.current_column = 0;
        } else {
            self.current_column += 1;
        }
        Some(current_char)
    }

    fn read_identifier(&mut self) -> String {
        let start_position = self.read_position;
        while self.current_char().is_some() && self.current_char().unwrap().is_alphabetic() {
            self.advance();
        }
        self.input[start_position..self.read_position].to_string()
    }

    fn read_number(&mut self) -> Token {
        let start_position = self.read_position;
        while self.current_char().is_some() && self.current_char().unwrap().is_numeric() {
            self.advance();
        }
        let number = self.input[start_position..self.read_position].to_string();
        if number.contains(".") {
            Token::FloatLiteral(number.parse::<f32>().unwrap())
        } else {
            Token::IntegerLiteral(number.parse::<i32>().unwrap())
        }
    }

    fn read_string(&mut self) -> String {
        // Skip the opening quote
        self.advance();
        let start_position = self.read_position;
        while self.current_char().is_some() && self.current_char().unwrap() != '"' {
            let current_char = self.advance();
            // if current_char.is_none() {

            // }
        }
        let string_literal = self.input[start_position..self.read_position].to_string();
        // Skip the closing quote
        self.advance();
        string_literal
    }

}

mod tests {

    use super::*;

    #[test]
    fn test_lexer_assignment() {
        let input = String::from("let x = 5 + 10;");
        let mut lexer = Lexer::new(input);
        let tokens = lexer.run();
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier(String::from("x")),
            Token::Equal,
            Token::IntegerLiteral(5),
            Token::Plus,
            Token::IntegerLiteral(10),
            Token::Terminator,
        ]);
    }

    #[test]
    fn test_lexer_string() {
        let input = String::from("let x = \"Hello World!\";");
        let mut lexer = Lexer::new(input);
        let tokens = lexer.run();
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier(String::from("x")),
            Token::Equal,
            Token::StringLiteral(String::from("Hello World!")),
            Token::Terminator,
        ]);
    }

    #[test]
    fn test_function_call() {
        let input = String::from("let x = add(5, 10);");
        let mut lexer = Lexer::new(input);
        let tokens = lexer.run();
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier(String::from("x")),
            Token::Equal,
            Token::Identifier(String::from("add")),
            Token::LeftParen,
            Token::IntegerLiteral(5),
            Token::Comma,
            Token::IntegerLiteral(10),
            Token::RightParen,
            Token::Terminator,
        ]);
    }

}
