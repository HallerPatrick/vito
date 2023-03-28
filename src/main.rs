mod lexer;
mod parser;
mod file_utils;

use file_utils::load_file;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let file_path = "test.vito";

    let file_contents = load_file(file_path);

    let mut lexer = Lexer::new(file_contents.clone());
    let tokens = lexer.run();

    println!("Tokens: {:?}", tokens);
    
    let ast = Parser::new(&tokens.clone()).parse();

    println!("AST: {:?}", ast);

}
