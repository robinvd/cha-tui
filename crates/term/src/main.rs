mod divider;
mod filter;
mod focus;
mod git;
mod jj;
mod keymap;
mod modal;
mod persistence;
mod project;
mod remote;
mod session;
mod sidebar;
mod status;
mod term_io;
mod vcs;

mod app;
mod cli;

use cli::Args;
use facet_args as args;

fn main() -> Result<(), miette::Report> {
    miette::set_panic_hook();

    let args: Args = args::from_std_args()?;
    app::run(args)
}
