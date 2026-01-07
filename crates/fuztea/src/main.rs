use std::io::{self, BufRead};

use chatui::Program;
use facet::Facet;
use facet_args as args;
use fuztea::{
    ColumnConfig, FuzzyFinder, FuzzyFinderEvent, FuzzyFinderMsg, map_event, update, view,
};

#[derive(Facet)]
struct Args {
    /// Field separator for splitting input into columns
    #[facet(args::named, args::short = 'd')]
    separator: Option<String>,

    /// Comma-separated list of column indices to search (1-indexed)
    #[facet(args::named, args::short = 'n')]
    nth: Option<String>,

    /// Comma-separated list of column names for header display
    #[facet(args::named, args::short = 'c')]
    columns: Option<String>,
}

impl Args {
    fn separator_or_default(&self) -> String {
        self.separator
            .as_ref()
            .cloned()
            .unwrap_or_else(|| "\t".to_string())
    }

    fn parse_nth(&self) -> Option<Vec<usize>> {
        self.nth.as_ref().map(|s| {
            s.split(',')
                .filter_map(|part| part.trim().parse::<usize>().ok())
                .collect()
        })
    }

    fn parse_columns(&self) -> Option<Vec<String>> {
        self.columns
            .as_ref()
            .map(|s| s.split(',').map(|part| part.trim().to_string()).collect())
    }
}

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    let args: Args = args::from_std_args()?;

    let mut config = ColumnConfig::default().with_separator(args.separator_or_default());

    if let Some(cols) = args.parse_columns() {
        config = config.with_column_names(cols);
    }

    if let Some(nth) = args.parse_nth() {
        config = config.with_search_columns(nth);
    }

    // Extractor for Row items: return the columns directly
    let extractor: fuztea::ColumnExtractor<Row> = |row: &Row| row.columns.clone();

    let (mut model, handle) = FuzzyFinder::with_config(extractor, config);
    let input_handle = handle.clone();
    let separator = args.separator_or_default();

    std::thread::spawn(move || {
        let stdin = io::stdin();
        for line in stdin.lock().lines().map_while(Result::ok) {
            let columns = line
                .split(&separator)
                .map(|s| s.to_string())
                .collect::<Vec<_>>();
            let row = Row {
                original: line,
                columns,
            };
            input_handle.push_item(row);
        }
    });

    let program = Program::new(
        &mut model,
        |model, msg| match update(model, msg, |m| m) {
            FuzzyFinderEvent::Continue => chatui::Transition::Continue,
            FuzzyFinderEvent::Cancel => chatui::Transition::Quit,
            FuzzyFinderEvent::Select(_) => chatui::Transition::Continue,
            FuzzyFinderEvent::Activate => chatui::Transition::Quit,
            FuzzyFinderEvent::Task(task) => chatui::Transition::Task(task),
        },
        |model| view(model, |msg: FuzzyFinderMsg| msg),
    )
    .map_event(map_event);

    program
        .run()
        .map_err(|err| color_eyre::eyre::eyre!(format!("{err:?}")))?;

    if let Some(output) = model.submission() {
        println!("{}", output.original);
    }

    Ok(())
}

#[derive(Clone, Debug, PartialEq)]
struct Row {
    original: String,
    columns: Vec<String>,
}

impl std::fmt::Display for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.original)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_args(args_slice: &[&str]) -> Result<Args, String> {
        args::from_slice(&args_slice[1..]).map_err(|e| format!("{:?}", e))
    }

    #[test]
    fn parse_no_arguments() {
        let args = parse_args(&["fuztea"]).expect("should parse");
        assert_eq!(args.separator_or_default(), "\t");
        assert_eq!(args.nth, None);
        assert_eq!(args.columns, None);
    }

    #[test]
    fn parse_separator_short_flag() {
        let args = parse_args(&["fuztea", "-d", ":"]).expect("should parse");
        assert_eq!(args.separator_or_default(), ":");
    }

    #[test]
    fn parse_separator_long_flag() {
        let args = parse_args(&["fuztea", "--separator", ":"]).expect("should parse");
        assert_eq!(args.separator_or_default(), ":");
    }

    #[test]
    fn parse_nth_short_flag() {
        let args = parse_args(&["fuztea", "-n", "1,3"]).expect("should parse");
        assert_eq!(args.nth, Some("1,3".to_string()));
        assert_eq!(args.parse_nth(), Some(vec![1, 3]));
    }

    #[test]
    fn parse_nth_long_flag() {
        let args = parse_args(&["fuztea", "--nth", "2,4,5"]).expect("should parse");
        assert_eq!(args.nth, Some("2,4,5".to_string()));
        assert_eq!(args.parse_nth(), Some(vec![2, 4, 5]));
    }

    #[test]
    fn parse_columns_short_flag() {
        let args = parse_args(&["fuztea", "-c", "name,path"]).expect("should parse");
        assert_eq!(args.columns, Some("name,path".to_string()));
        assert_eq!(
            args.parse_columns(),
            Some(vec!["name".to_string(), "path".to_string()])
        );
    }

    #[test]
    fn parse_columns_long_flag() {
        let args = parse_args(&["fuztea", "--columns", "a,b,c"]).expect("should parse");
        assert_eq!(args.columns, Some("a,b,c".to_string()));
        assert_eq!(
            args.parse_columns(),
            Some(vec!["a".to_string(), "b".to_string(), "c".to_string()])
        );
    }

    #[test]
    fn parse_all_flags_together() {
        let args = parse_args(&[
            "fuztea",
            "-d",
            "|",
            "--nth",
            "1,2",
            "--columns",
            "name,type",
        ])
        .expect("should parse");
        assert_eq!(args.separator_or_default(), "|");
        assert_eq!(args.parse_nth(), Some(vec![1, 2]));
        assert_eq!(
            args.parse_columns(),
            Some(vec!["name".to_string(), "type".to_string()])
        );
    }

    #[test]
    fn parse_nth_with_spaces() {
        let args = parse_args(&["fuztea", "-n", "1, 2, 3"]).expect("should parse");
        assert_eq!(args.parse_nth(), Some(vec![1, 2, 3]));
    }

    #[test]
    fn parse_columns_with_spaces() {
        let args = parse_args(&["fuztea", "-c", "name , path , type"]).expect("should parse");
        assert_eq!(
            args.parse_columns(),
            Some(vec![
                "name".to_string(),
                "path".to_string(),
                "type".to_string()
            ])
        );
    }

    #[test]
    fn parse_nth_handles_invalid_numbers() {
        let args = parse_args(&["fuztea", "-n", "1,invalid,3"]).expect("should parse");
        assert_eq!(args.parse_nth(), Some(vec![1, 3]));
    }

    #[test]
    fn parse_nth_empty_string() {
        let args = parse_args(&["fuztea", "-n", ""]).expect("should parse");
        assert_eq!(args.parse_nth(), Some(vec![]));
    }

    #[test]
    fn parse_columns_empty_string() {
        let args = parse_args(&["fuztea", "-c", ""]).expect("should parse");
        assert_eq!(args.parse_columns(), Some(vec!["".to_string()]));
    }

    #[test]
    fn parse_multiple_separators_in_value() {
        let args = parse_args(&["fuztea", "-d", ", "]).expect("should parse");
        assert_eq!(args.separator_or_default(), ", ");
    }

    #[test]
    fn parse_nth_single_column() {
        let args = parse_args(&["fuztea", "--nth", "1"]).expect("should parse");
        assert_eq!(args.parse_nth(), Some(vec![1]));
    }

    #[test]
    fn parse_columns_single_column() {
        let args = parse_args(&["fuztea", "--columns", "name"]).expect("should parse");
        assert_eq!(args.parse_columns(), Some(vec!["name".to_string()]));
    }

    #[test]
    fn parse_separator_with_special_chars() {
        let args = parse_args(&["fuztea", "-d", "\\n"]).expect("should parse");
        assert_eq!(args.separator_or_default(), "\\n");
    }

    #[test]
    fn parse_combined_short_flags() {
        let args =
            parse_args(&["fuztea", "-d", ":", "-n", "1,2", "-c", "a,b"]).expect("should parse");
        assert_eq!(args.separator_or_default(), ":");
        assert_eq!(args.parse_nth(), Some(vec![1, 2]));
        assert_eq!(
            args.parse_columns(),
            Some(vec!["a".to_string(), "b".to_string()])
        );
    }

    #[test]
    fn parse_nth_multiple_values() {
        let args = parse_args(&["fuztea", "-n", "1,2,3,4,5"]).expect("should parse");
        assert_eq!(args.parse_nth(), Some(vec![1, 2, 3, 4, 5]));
    }

    #[test]
    fn parse_columns_multiple_values() {
        let args =
            parse_args(&["fuztea", "-c", "first,second,third,fourth"]).expect("should parse");
        assert_eq!(
            args.parse_columns(),
            Some(vec![
                "first".to_string(),
                "second".to_string(),
                "third".to_string(),
                "fourth".to_string()
            ])
        );
    }

    #[test]
    fn parse_separator_default_when_not_provided() {
        let args = parse_args(&["fuztea", "-n", "1"]).expect("should parse");
        assert_eq!(args.separator_or_default(), "\t");
    }

    #[test]
    fn parse_multiple_flags_mixed_order() {
        let args =
            parse_args(&["fuztea", "--nth", "2,3", "-d", ";", "-c", "x,y"]).expect("should parse");
        assert_eq!(args.separator_or_default(), ";");
        assert_eq!(args.parse_nth(), Some(vec![2, 3]));
        assert_eq!(
            args.parse_columns(),
            Some(vec!["x".to_string(), "y".to_string()])
        );
    }
}
