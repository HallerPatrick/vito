use std::fs::File;
use std::io::prelude::*;

pub fn load_file(file_path: &str) -> String {
    let mut file = File::open(file_path).expect("File not found");
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("Something went wrong reading the file");
    contents
}