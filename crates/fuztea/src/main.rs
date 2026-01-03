use std::io::{self, Read, Write};

use chatui::Program;
use fuztea::{FuzzyFinder, map_event, update, view};

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let items = input.lines().map(|line| line.to_string()).collect();

    let (model, handle) = FuzzyFinder::new(items);
    let program = Program::new(model, update, view).map_event(map_event);

    program
        .run()
        .map_err(|err| color_eyre::eyre::eyre!(format!("{err:?}")))?;

    if let Some(output) = handle.take_submission() {
        let mut stdout = io::stdout();
        writeln!(stdout, "{output}")?;
    }

    Ok(())
}
