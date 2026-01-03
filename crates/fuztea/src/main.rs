use std::io::{self, BufRead, Write};

use chatui::Program;
use fuztea::{FuzzyFinder, map_event, update, view};

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let (model, handle) = FuzzyFinder::new();
    let input_handle = handle.clone();
    std::thread::spawn(move || {
        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            if let Ok(line) = line {
                input_handle.push_item(line);
            }
        }
    });

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
