use compiler::Compiler;

mod compiler;
mod errors;
mod parser;
mod typechecker;

use parser::Parser;
use typechecker::Typechecker;

fn compile(fname: &str, mut compiler: Compiler) -> Compiler {
    let debug_output = true;

    let contents = std::fs::read(fname).unwrap();

    let span_offset = compiler.span_offset();
    let node_id_offset = compiler.node_id_offset();
    compiler.add_file(fname, &contents);

    let parser = Parser::new(compiler, span_offset, node_id_offset);
    let compiler = parser.parse();

    for error in &compiler.errors.first() {
        compiler.print_error(error)
    }

    if !compiler.errors.is_empty() {
        return compiler;
    }

    let typechecker = Typechecker::new(compiler);
    let compiler = typechecker.typecheck();

    for error in &compiler.errors.first() {
        compiler.print_error(error)
    }

    if !compiler.errors.is_empty() {
        return compiler;
    }

    if debug_output {
        println!();
        println!("Results:");
        compiler.print();
    }

    compiler
}

fn main() {
    let mut compiler = Compiler::new();

    for fname in std::env::args().skip(1) {
        compiler = compile(&fname, compiler);
    }
}
