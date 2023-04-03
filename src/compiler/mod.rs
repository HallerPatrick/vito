mod instructions;

use crate::compiler::instructions::Instruction;

use crate::lexer::Token;
use crate::parser::{ Parser, Statement, Block };

pub struct Compiler {
    pub tokens: Vec<Token>,
    pub ast: Block
}

impl Compiler {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            ast: Block::new()
        }
    }

    fn parse_ast(&mut self) -> Result<(), String> {
        let mut parser = Parser::new(&self.tokens);
        let ast = parser.parse();

        match ast {
            Ok(ast) => {
                self.ast = ast;
                Ok(())
            },
            Err(e) => Err(e.to_string())
        }
    }

    /// Compile th AST into bytecode
    pub fn compile(&mut self) -> Result<(), String> {
        self.parse_ast()?;

        let mut bytecode: Vec<Instruction> = vec![];

        for node in self.ast.statements.iter() {
            match node {
                // Statement::Assignment(assignment) => {
                //     bytecode.push(Instruction::Push(assignment.expression.literal.clone().unwrap().into()));
                //     bytecode.push(Instruction::Pop);
                // },
                Statement::FunctionCall(identifier, args) => {
                    for arg in args.iter() {
                        // bytecode.push(Instruction::Push(arg.literal.clone().unwrap().into()));
                    }
                    // bytecode.push(Instruction::Call(identifier.name.clone()));
                },
                _ => ()
            }
        }


        Ok(())
    }

}