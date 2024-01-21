mod codegen;
mod compiler;
mod errors;
mod lifetime_checker;
mod parser;
mod typechecker;

use std::process::exit;

use compiler::Compiler;
use lifetime_checker::LifetimeChecker;
use parser::Parser;
use typechecker::Typechecker;

fn compile(fname: &str, mut compiler: Compiler) -> Compiler {
    let debug_output = false;

    let contents = std::fs::read(fname);

    let Ok(contents) = contents else {
        eprintln!("can't find {}", fname);
        exit(1);
    };

    let span_offset = compiler.span_offset();
    compiler.add_file(fname, &contents);

    let parser = Parser::new(compiler, span_offset);
    let compiler = parser.parse();

    let mut first_error = true;

    for error in &compiler.errors {
        if !first_error {
            eprintln!();
        } else {
            first_error = true;
        }
        compiler.print_error(error)
    }

    if !compiler.errors.is_empty() {
        std::process::exit(1);
    }

    let typechecker = Typechecker::new(compiler);
    let compiler = typechecker.typecheck();

    for error in &compiler.errors {
        if !first_error {
            eprintln!();
        } else {
            first_error = true;
        }
        compiler.print_error(error)
    }

    if !compiler.errors.is_empty() {
        std::process::exit(1);
    }

    let lifetime_checker = LifetimeChecker::new(compiler);
    let compiler = lifetime_checker.check_lifetimes();

    for error in &compiler.errors {
        if !first_error {
            eprintln!();
        } else {
            first_error = true;
        }
        compiler.print_error(error)
    }

    if debug_output {
        println!();
        println!("Results:");
        compiler.print();
    }

    if !compiler.errors.is_empty() {
        std::process::exit(1);
    }

    compiler
}

fn main() {
    let mut compiler = Compiler::new();

    for fname in std::env::args().skip(1) {
        compiler = compile(&fname, compiler);
    }

    let codegen = codegen::Codegen::new(compiler);

    let output = codegen.codegen();
    println!("{}", String::from_utf8_lossy(&output));
}
