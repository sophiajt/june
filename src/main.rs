mod codegen;
mod compiler;
mod errors;
mod lifetime_checker;
mod parser;
mod typechecker;

// cli submodules
mod build;
mod new;
mod run;

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
    Run(run::Args),
}

impl Cli {
    pub fn parse() -> Self {
        clap::Parser::parse()
    }

    fn project_root(&self) -> Result<PathBuf, ProjectRootError> {
        let curr = std::env::current_dir().map_err(ProjectRootError::CurrentDir)?;

        for path in curr.ancestors() {
            if path.join("June.toml").is_file() {
                return Ok(path.to_path_buf());
            }
        }

        Err(ProjectRootError::JuneToml)
    }

    fn new_project(&self, args: &new::Args) -> Result<(), CLIError> {
        let main_dir = args.path.join("main");
        // create the directory
        // create the main subdirectory
        std::fs::create_dir_all(&main_dir).map_err(NewProjectError::MainDir)?;
        // .expect("path should be a valid path the user can create");
        // create the hello world main.june file
        let hello_world = r#"println(c"hello")"#;
        std::fs::write(main_dir.join("main.june"), hello_world)
            .map_err(NewProjectError::MainJune)?;
        // create the empty June.toml file in the root
        std::fs::File::create(args.path.join("June.toml")).map_err(NewProjectError::JuneToml)?;
        // initialize git vcs
        let _cmd = std::process::Command::new("git")
            .arg("init")
            .arg("-b")
            .arg("main")
            .current_dir(&args.path)
            .output()
            .map_err(NewProjectError::GitInit)?;

        Ok(())
    }

    fn build(&self, _args: &build::Args) -> Result<(), CLIError> {
        let root_dir = self.project_root().map_err(BuildProjectError::from)?;
        let main_root = root_dir.join("main").join("main.june");
        let build_dir = root_dir.join("build").join("debug");
        std::fs::create_dir_all(&build_dir).map_err(BuildProjectError::BuildDir)?;

        if main_root.is_file() {
            let mut compiler = Compiler::new();
            compiler = compile(&main_root, compiler);
            let codegen = codegen::Codegen::new(compiler);

            let output = codegen.codegen();

            let c_output_filepath = build_dir.join("main.c");
            // TODO: get actual project name from toml file
            let app_filepath = build_dir.join("main");
            let mut output_file =
                File::create(&c_output_filepath).map_err(BuildProjectError::MainCCreate)?;
            output_file
                .write_all(&output)
                .map_err(BuildProjectError::MainCWrite)?;

            // Next, compile the file
            let compiler = std::process::Command::new("clang")
                .arg(&c_output_filepath)
                .arg("-o")
                .arg(&app_filepath)
                .output()
                .map_err(BuildProjectError::Clang)?;

            if !compiler.status.success() {
                let _ = std::io::stdout().write_all(&compiler.stdout);
                let _ = std::io::stdout().write_all(&compiler.stderr);
                panic!("Clang did not compile successfully");
            }
        }

        Ok(())
    }

    fn run(&self, args: &run::Args) -> Result<(), CLIError> {
        let project_root = self.project_root().map_err(RunProjectError::from)?;
        let cmd = project_root.join("build").join("debug").join("main");
        let status = std::process::Command::new(cmd)
            .args(&args.user_args)
            .status()
            .map_err(RunProjectError::Subprocess)?;

        if let Some(code) = status.code() {
            std::process::exit(code);
        }

        Ok(())
    }
}

#[derive(Debug)]
enum CLIError {
    New(NewProjectError),
    Build(BuildProjectError),
    Run(RunProjectError),
}

impl fmt::Display for CLIError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CLIError::New(_) => f.write_str("cannot to create new project"),
            CLIError::Build(_) => f.write_str("cannot to compile project"),
            CLIError::Run(_) => f.write_str("cannot to run project"),
        }
    }
}

impl Error for CLIError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            CLIError::New(src) => Some(src),
            CLIError::Build(src) => Some(src),
            CLIError::Run(src) => Some(src),
        }
    }
}

impl From<NewProjectError> for CLIError {
    fn from(value: NewProjectError) -> Self {
        Self::New(value)
    }
}

impl From<BuildProjectError> for CLIError {
    fn from(value: BuildProjectError) -> Self {
        Self::Build(value)
    }
}

impl From<RunProjectError> for CLIError {
    fn from(value: RunProjectError) -> Self {
        Self::Run(value)
    }
}

#[derive(Debug)]
enum ProjectRootError {
    CurrentDir(io::Error),
    JuneToml,
}

impl fmt::Display for ProjectRootError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProjectRootError::CurrentDir(_) => f.write_str("cannot to access current directory"),
            ProjectRootError::JuneToml => f.write_str("June.toml file not found"),
        }
    }
}

impl Error for ProjectRootError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ProjectRootError::CurrentDir(src) => Some(src),
            ProjectRootError::JuneToml => None,
        }
    }
}

#[derive(Debug)]
enum NewProjectError {
    MainDir(io::Error),
    MainJune(io::Error),
    JuneToml(io::Error),
    GitInit(io::Error),
}

impl fmt::Display for NewProjectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NewProjectError::MainDir(_) => f.write_str("cannot to create main directory"),
            NewProjectError::MainJune(_) => f.write_str("cannot to create main.june file"),
            NewProjectError::JuneToml(_) => f.write_str("cannot to create June.toml file"),
            NewProjectError::GitInit(_) => f.write_str("cannot to initialize git repository"),
        }
    }
}

impl Error for NewProjectError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            NewProjectError::MainDir(src) => Some(src),
            NewProjectError::MainJune(src) => Some(src),
            NewProjectError::JuneToml(src) => Some(src),
            NewProjectError::GitInit(src) => Some(src),
        }
    }
}

#[derive(Debug)]
enum BuildProjectError {
    NoProjectRoot(ProjectRootError),
    BuildDir(io::Error),
    MainCCreate(io::Error),
    MainCWrite(io::Error),
    Clang(io::Error),
}

impl fmt::Display for BuildProjectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuildProjectError::NoProjectRoot(_) => {
                f.write_str("Current directory does not appear to be part of a valid June project")
            }
            BuildProjectError::BuildDir(_) => f.write_str("cannot create build directory"),
            BuildProjectError::MainCCreate(_) => {
                f.write_str("cannot create temporary file for c codegen")
            }
            BuildProjectError::MainCWrite(_) => f.write_str("cannot write to codegen file"),
            BuildProjectError::Clang(_) => f.write_str("cannot compile codegen into binary"),
        }
    }
}

impl Error for BuildProjectError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            BuildProjectError::NoProjectRoot(src) => Some(src),
            BuildProjectError::BuildDir(src) => Some(src),
            BuildProjectError::MainCCreate(src) => Some(src),
            BuildProjectError::MainCWrite(src) => Some(src),
            BuildProjectError::Clang(src) => Some(src),
        }
    }
}

impl From<ProjectRootError> for BuildProjectError {
    fn from(value: ProjectRootError) -> Self {
        Self::NoProjectRoot(value)
    }
}

#[derive(Debug)]
enum RunProjectError {
    NoProjectRoot(ProjectRootError),
    Subprocess(io::Error),
}

impl fmt::Display for RunProjectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RunProjectError::NoProjectRoot(_) => {
                f.write_str("Current directory does not appear to be part of a valid June project")
            }
            RunProjectError::Subprocess(_) => f.write_str("cannot invoke executable"),
        }
    }
}

impl Error for RunProjectError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            RunProjectError::Subprocess(src) => Some(src),
            RunProjectError::NoProjectRoot(src) => Some(src),
        }
    }
}

impl From<ProjectRootError> for RunProjectError {
    fn from(value: ProjectRootError) -> Self {
        Self::NoProjectRoot(value)
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

    let result = match &cli.command {
        Some(command) => match command {
            Command::Build(args) => cli.build(args),
            Command::New(args) => cli.new_project(args),
            Command::Run(args) => cli.run(args),
        },
        None => {
            for fname in &cli.files {
                compiler = compile(fname, compiler);
            }

            let codegen = codegen::Codegen::new(compiler);

            let output = codegen.codegen();
            println!("{}", String::from_utf8_lossy(&output));
            Ok(())
        }
    };

    match result {
        Ok(()) => {}
        Err(error) => {
            eprintln!("Error:");
            let error: &(dyn Error + 'static) = &error;
            let chain = std::iter::successors(Some(error), |&e| e.source());
            for (ind, e) in chain.enumerate() {
                eprintln!("   {ind}: {e}");
            }
        }
    }
}
