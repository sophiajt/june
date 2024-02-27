mod codegen;
mod compiler;
mod errors;
mod lifetime_checker;
mod parser;
mod typechecker;

use compiler::Compiler;
use lifetime_checker::LifetimeChecker;
use parser::Parser;
use tracing_subscriber::prelude::*;
use tracing_subscriber::EnvFilter;
use typechecker::Typechecker;

fn compile(fname: &str, mut compiler: Compiler) -> Compiler {
    let debug_output = true;

    let span_offset = compiler.span_offset();
    compiler.add_file(fname);

    let parser = Parser::new(compiler, span_offset);
    let compiler = parser.parse();

    for error in &compiler.errors {
        compiler.print_error(error)
    }

    if !compiler.errors.is_empty() {
        std::process::exit(1);
    }

    let typechecker = Typechecker::new(compiler);
    let compiler = typechecker.typecheck();

    for error in &compiler.errors {
        compiler.print_error(error)
    }

    if !compiler.errors.is_empty() {
        std::process::exit(1);
    }

    let lifetime_checker = LifetimeChecker::new(compiler);
    let compiler = lifetime_checker.check_lifetimes();

    for error in &compiler.errors {
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
    let fmt_layer = tracing_tree::HierarchicalLayer::default()
        .with_writer(std::io::stderr)
        .with_indent_lines(true)
        .with_targets(true)
        .with_indent_amount(2);
    let filter_layer = EnvFilter::try_from_default_env()
        .or_else(|_| EnvFilter::try_new("info"))
        .unwrap();

    tracing_subscriber::registry()
        .with(filter_layer)
        .with(fmt_layer)
        .init();

    let mut compiler = Compiler::new();

    for fname in std::env::args().skip(1) {
        compiler = compile(&fname, compiler);
    }

    let codegen = codegen::Codegen::new(compiler);

    let output = codegen.codegen();
    println!("{}", String::from_utf8_lossy(&output));
}
