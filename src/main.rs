mod lexer;
mod parser;
mod file_utils;
mod compiler;

use file_utils::load_file;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let file_path = "test.vito";

    let file_contents = load_file(file_path);

    let mut lexer = Lexer::new(file_contents.clone());
    let tokens = lexer.run();

    println!("Tokens: {:?}", tokens);
    
    let mut parser = Parser::new(&tokens);
    let ast = parser.parse();

    match ast {
        Ok(ast) => println!("AST: {:?}", ast),
        Err(e) => eprintln!("Error: {:?}", e.to_string())
    }

}
