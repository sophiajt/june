#[derive(clap::Args, Debug)]
pub struct Args {
    #[command(flatten)]
    pub build_args: super::build::Args,

    #[arg(last = true)]
    pub user_args: Vec<String>,
}
