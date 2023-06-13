use compiler::Compiler;

mod compiler;
mod errors;
mod parser;
mod typechecker;

fn main() {
    let debug_output = true;

    for file in std::env::args().skip(1) {
        let contents = std::fs::read(&file).unwrap();
        let mut compiler = Compiler::new();

        let span_offset = compiler.span_offset();
        let node_id_offset = compiler.node_id_offset();
        compiler.add_file(&file, &contents);

        let parser = parser::Parser::new(compiler, span_offset, node_id_offset);
        let compiler = parser.parse();

        for error in &compiler.errors.first() {
            compiler.print_error(error)
        }

        if !compiler.errors.is_empty() {
            return;
        }

        if debug_output {
            println!();
            println!("parse result:");
            compiler.print();
        }
    }
}
