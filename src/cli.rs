use clap::Parser;

/// The June compiler
#[derive(Parser, Debug)]
#[command(version, about)]
pub struct Args {
    /// Files to compile
    pub files: Vec<String>,
}

impl Args {
    pub fn parse() -> Self {
        Parser::parse()
    }
}
