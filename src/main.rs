mod codegen;
mod compiler;
mod errors;
mod lifetime_checker;
mod parser;
mod typechecker;

// cli submodules
mod build;
mod new;

use core::fmt;
use std::error::Error;
use std::fs::File;
use std::io;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

use compiler::Compiler;
use lifetime_checker::LifetimeChecker;
use parser::Parser;
use tracing_subscriber::prelude::*;
use tracing_subscriber::EnvFilter;
use typechecker::Typechecker;

/// The June compiler
#[derive(clap::Parser, Debug)]
#[command(version, about)]
pub struct Cli {
    /// Files to compile
    pub files: Vec<PathBuf>,

    #[command(subcommand)]
    pub command: Option<Command>,
}

#[derive(clap::Subcommand, Debug)]
pub enum Command {
    Build(build::Args),
    New(new::Args),
}

impl Cli {
    pub fn parse() -> Self {
        clap::Parser::parse()
    }

    pub fn project_root(&self) -> Option<PathBuf> {
        let curr = std::env::current_dir()
            .expect("june CLI should always be run from within a valid june project");

        for path in curr.ancestors() {
            if path.join("June.toml").is_file() {
                return Some(path.to_path_buf());
            }
        }

        None
    }

    fn build(&self, _args: &build::Args) {
        let root_dir = self
            .project_root()
            .expect("june build should only be run in valid june projects");
        // let lib_root = root_dir.join("lib").join("lib.june");
        let main_root = root_dir.join("main").join("main.june");
        let build_dir = root_dir.join("build").join("debug");
        std::fs::create_dir_all(&build_dir).unwrap();

        if main_root.is_file() {
            let mut compiler = Compiler::new();
            compiler = compile(&main_root, compiler);
            let codegen = codegen::Codegen::new(compiler);

            let output = codegen.codegen();
            // println!("{}", String::from_utf8_lossy(&output));

            let c_output_filepath = build_dir.join("main.c");
            // TODO: get actual project name from toml file
            let app_filepath = build_dir.join("main");
            let mut output_file = File::create(&c_output_filepath).unwrap();
            let _ = output_file.write_all(&output);

            // Next, compile the file
            let compiler = std::process::Command::new("clang")
                .arg(&c_output_filepath)
                .arg("-o")
                .arg(&app_filepath)
                .output()
                .unwrap();

            if !compiler.status.success() {
                let _ = std::io::stdout().write_all(&compiler.stdout);
                let _ = std::io::stdout().write_all(&compiler.stderr);
                panic!("Clang did not compile successfully");
            }
        }
    }

    fn new_project(&self, args: &new::Args) -> Result<(), NewProjectError> {
        let main_dir = args.path.join("main");
        // create the directory
        // create the main subdirectory
        std::fs::create_dir_all(&main_dir)?;
        // .expect("path should be a valid path the user can create");
        // create the hello world main.june file
        let hello_world = r#"println(c"hello")"#;
        std::fs::write(main_dir.join("main.june"), hello_world)?;
        // create the empty June.toml file in the root
        std::fs::File::create(args.path.join("June.toml"))?;
        // initialize git vcs
        let _cmd = std::process::Command::new("git")
            .arg("init")
            .arg("-b")
            .arg("main")
            .current_dir(&args.path)
            .output()?;

        Ok(())
    }
}

#[derive(Debug)]
struct NewProjectError(io::Error);

impl fmt::Display for NewProjectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("cannot create new project")
    }
}

impl Error for NewProjectError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(&self.0)
    }
}

impl From<io::Error> for NewProjectError {
    fn from(value: io::Error) -> Self {
        Self(value)
    }
}

fn compile(fname: &Path, mut compiler: Compiler) -> Compiler {
    let debug_output = false;

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
    let cli = Cli::parse();
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

    match &cli.command {
        Some(command) => match command {
            Command::Build(args) => cli.build(args),
            Command::New(args) => cli.new_project(args).unwrap(),
        },
        None => {
            for fname in &cli.files {
                compiler = compile(fname, compiler);
            }

            let codegen = codegen::Codegen::new(compiler);

            let output = codegen.codegen();
            println!("{}", String::from_utf8_lossy(&output));
        }
    }
}
