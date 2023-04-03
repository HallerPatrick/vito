// Bytecode instructions for the compiler

pub enum Instruction {
    // Push a value onto the stack
    Push(i32),
    // Pop a value off the stack
    Pop,
    // Add the top two values on the stack
    Add,
    // Subtract the top two values on the stack
    Sub,
    // Multiply the top two values on the stack
    Mul,
    // Divide the top two values on the stack
    Div,
    // Print the top value on the stack
    Print,
    // Halt the program
    Halt
}

