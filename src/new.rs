use std::path::PathBuf;

#[derive(clap::Args, Debug)]
/// Create a new june package at <path>
pub struct Args {
    pub path: PathBuf,
}
