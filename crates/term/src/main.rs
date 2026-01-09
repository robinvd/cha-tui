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

use cli::{Args, Command, RemoteArgs};
use facet_args as args;

fn main() -> Result<(), miette::Report> {
    miette::set_panic_hook();

    let args: Args = args::from_std_args()?;
    if let Some(Command::Remote {
        json,
        text,
        socket,
        action,
    }) = args.command
    {
        let client_args = crate::cli::build_remote_client_args(RemoteArgs {
            json,
            text,
            socket,
            action,
        });
        if let Err(err) = crate::remote::run_remote(client_args) {
            eprintln!("error from term server: {err}");
            std::process::exit(1);
        }
        return Ok(());
    }

    app::run()
}
