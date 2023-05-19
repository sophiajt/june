mod delta;
mod errors;
mod lexer;
mod parser;

fn main() {
    let debug_output = true;

    for file in std::env::args().skip(1) {
        let contents = std::fs::read_to_string(file).unwrap();
        let mut parser = parser::Parser::new(contents.as_bytes(), 0, 0);
        parser.parse();

        for error in &parser.errors {
            println!("error: {:?}", error);
        }

        if !parser.errors.is_empty() {
            return;
        }

        let result = &parser.delta;

        if debug_output {
            println!();
            println!("parse result:");
            result.print();
        }
    }
}
