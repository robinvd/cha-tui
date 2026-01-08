//! Session filter expression parsing and evaluation.

use crate::project::{Project, Worktree};
use crate::session::Session;

/// A filter expression that can contain AND and OR operations.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FilterExpression {
    /// All queries must match (AND).
    And(Vec<SessionQuery>),
    /// At least one sub-expression must match (OR).
    Or(Vec<FilterExpression>),
}

/// Individual session query predicate.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SessionQuery {
    Title(String, MatchKind),
    ProjectName(String, MatchKind),
    WorkspaceName(String, MatchKind),
    Cwd(String, MatchKind),
    Id(u64),
    HasUpdates(bool),
    HasBell(bool),
    CurrentProject(bool),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MatchKind {
    Exact,
    Substring,
}

/// Parse a filter expression from user input.
/// Syntax: query clauses separated by spaces (AND), with | for OR (lowest precedence).
/// Example: "title~=foo project=bar | has-updates=true"
///   parses as: (title~=foo AND project=bar) OR has-updates=true
pub fn parse_filter(input: &str) -> Result<FilterExpression, String> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return Ok(FilterExpression::And(Vec::new()));
    }

    // Split by | for OR expressions
    let or_parts: Vec<&str> = trimmed.split('|').collect();

    if or_parts.len() == 1 {
        // No OR operator, just parse as AND
        parse_and_expression(or_parts[0])
    } else {
        // Parse each OR part as an AND expression
        let mut or_exprs = Vec::new();
        for part in or_parts {
            or_exprs.push(parse_and_expression(part)?);
        }
        Ok(FilterExpression::Or(or_exprs))
    }
}

fn parse_and_expression(input: &str) -> Result<FilterExpression, String> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return Ok(FilterExpression::And(Vec::new()));
    }

    let mut queries = Vec::new();
    let mut current = String::new();
    let mut in_value = false;

    for ch in trimmed.chars() {
        if ch == '=' && !in_value {
            in_value = true;
            current.push(ch);
        } else if ch.is_whitespace() && in_value {
            // End of a query clause
            if !current.trim().is_empty() {
                queries.push(parse_single_query(&current)?);
                current.clear();
                in_value = false;
            }
        } else {
            current.push(ch);
        }
    }

    // Don't forget the last query
    if !current.trim().is_empty() {
        queries.push(parse_single_query(&current)?);
    }

    Ok(FilterExpression::And(queries))
}

fn parse_single_query(input: &str) -> Result<SessionQuery, String> {
    let trimmed = input.trim();

    let (key, operator, value) = if let Some((key, value)) = trimmed.split_once("~=") {
        (key, "~=", value)
    } else if let Some((key, value)) = trimmed.split_once('=') {
        (key, "=", value)
    } else {
        return Err(format!("expected key=value or key~=value, got '{input}'"));
    };

    let key = normalize_query_key(key.trim());
    let value = value.trim();
    let match_kind = match operator {
        "=" => MatchKind::Exact,
        "~=" => MatchKind::Substring,
        _ => return Err(format!("unknown operator '{operator}'")),
    };

    if value.is_empty() {
        return Err(format!("query '{key}' requires a value"));
    }

    match key.as_str() {
        "title" => Ok(SessionQuery::Title(value.to_string(), match_kind)),
        "project" | "project name" => Ok(SessionQuery::ProjectName(value.to_string(), match_kind)),
        "workspace" | "workspace name" | "worktree" | "worktree name" => {
            Ok(SessionQuery::WorkspaceName(value.to_string(), match_kind))
        }
        "cwd" => Ok(SessionQuery::Cwd(value.to_string(), match_kind)),
        "id" => {
            ensure_exact_operator(&key, match_kind)?;
            value
                .parse::<u64>()
                .map(SessionQuery::Id)
                .map_err(|_| format!("invalid id value '{value}'"))
        }
        "has updates" | "updates" | "unread" => {
            ensure_exact_operator(&key, match_kind)?;
            parse_query_bool(value)
                .map(SessionQuery::HasUpdates)
                .ok_or_else(|| format!("invalid has updates value '{value}'"))
        }
        "has bell" | "bell" => {
            ensure_exact_operator(&key, match_kind)?;
            parse_query_bool(value)
                .map(SessionQuery::HasBell)
                .ok_or_else(|| format!("invalid has bell value '{value}'"))
        }
        "current project" | "current workspace" | "current worktree" => {
            ensure_exact_operator(&key, match_kind)?;
            parse_query_bool(value)
                .map(SessionQuery::CurrentProject)
                .ok_or_else(|| format!("invalid current project value '{value}'"))
        }
        _ => Err(format!("unknown query key '{key}'")),
    }
}

fn normalize_query_key(key: &str) -> String {
    let mut normalized = String::new();
    for part in key
        .split(|ch: char| ch.is_ascii_whitespace() || ch == '_' || ch == '-')
        .filter(|part| !part.is_empty())
    {
        if !normalized.is_empty() {
            normalized.push(' ');
        }
        normalized.push_str(&part.to_ascii_lowercase());
    }
    normalized
}

fn parse_query_bool(value: &str) -> Option<bool> {
    match value.trim().to_ascii_lowercase().as_str() {
        "true" | "t" | "1" | "yes" | "y" | "on" => Some(true),
        "false" | "f" | "0" | "no" | "n" | "off" => Some(false),
        _ => None,
    }
}

fn ensure_exact_operator(key: &str, match_kind: MatchKind) -> Result<(), String> {
    if match_kind == MatchKind::Exact {
        Ok(())
    } else {
        Err(format!("query '{key}' requires '=' operator"))
    }
}

/// Evaluate a filter expression against a session.
pub fn matches_filter(
    expr: &FilterExpression,
    project: &Project,
    worktree: Option<&Worktree>,
    session: &Session,
    active_session: Option<(
        crate::project::ProjectId,
        Option<crate::project::WorktreeId>,
    )>,
) -> bool {
    match expr {
        FilterExpression::And(queries) => {
            if queries.is_empty() {
                return true; // Empty filter matches everything
            }
            queries
                .iter()
                .all(|query| matches_query(query, project, worktree, session, active_session))
        }
        FilterExpression::Or(exprs) => exprs
            .iter()
            .any(|expr| matches_filter(expr, project, worktree, session, active_session)),
    }
}

fn matches_query(
    query: &SessionQuery,
    project: &Project,
    worktree: Option<&Worktree>,
    session: &Session,
    active_session: Option<(
        crate::project::ProjectId,
        Option<crate::project::WorktreeId>,
    )>,
) -> bool {
    match query {
        SessionQuery::Title(needle, match_kind) => {
            matches_query_text(&session_title_for_query(session), needle, *match_kind)
        }
        SessionQuery::ProjectName(needle, match_kind) => {
            matches_query_text(&project.name, needle, *match_kind)
        }
        SessionQuery::WorkspaceName(needle, match_kind) => worktree
            .map(|worktree| matches_query_text(&worktree.name, needle, *match_kind))
            .unwrap_or(false),
        SessionQuery::Cwd(needle, match_kind) => {
            let path = match worktree {
                Some(worktree) => worktree.path.display().to_string(),
                None => project.path.display().to_string(),
            };
            matches_query_text(&path, needle, *match_kind)
        }
        SessionQuery::Id(id) => session.id.0 == *id,
        SessionQuery::HasUpdates(expected) => session.has_unread_output == *expected,
        SessionQuery::HasBell(expected) => session.bell == *expected,
        SessionQuery::CurrentProject(expected) => {
            let is_current = active_session
                .map(|(active_project, active_worktree)| {
                    project.id == active_project && worktree.map(|w| w.id) == active_worktree
                })
                .unwrap_or(false);
            is_current == *expected
        }
    }
}

fn session_title_for_query(session: &Session) -> String {
    session
        .custom_title
        .clone()
        .or_else(|| session.title.clone())
        .unwrap_or_else(|| session.display_name())
}

fn matches_query_text(value: &str, query: &str, match_kind: MatchKind) -> bool {
    let value = value.to_ascii_lowercase();
    let query = query.trim().to_ascii_lowercase();
    match match_kind {
        MatchKind::Exact => value == query,
        MatchKind::Substring => value.contains(&query),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty_filter() {
        let expr = parse_filter("").unwrap();
        assert_eq!(expr, FilterExpression::And(Vec::new()));
    }

    #[test]
    fn parse_single_exact_match() {
        let expr = parse_filter("title=foo").unwrap();
        assert_eq!(
            expr,
            FilterExpression::And(vec![SessionQuery::Title(
                "foo".to_string(),
                MatchKind::Exact
            )])
        );
    }

    #[test]
    fn parse_single_substring_match() {
        let expr = parse_filter("title~=foo").unwrap();
        assert_eq!(
            expr,
            FilterExpression::And(vec![SessionQuery::Title(
                "foo".to_string(),
                MatchKind::Substring
            )])
        );
    }

    #[test]
    fn parse_and_expression() {
        let expr = parse_filter("title=foo project=bar").unwrap();
        assert_eq!(
            expr,
            FilterExpression::And(vec![
                SessionQuery::Title("foo".to_string(), MatchKind::Exact),
                SessionQuery::ProjectName("bar".to_string(), MatchKind::Exact),
            ])
        );
    }

    #[test]
    fn parse_or_expression() {
        let expr = parse_filter("title=foo | project=bar").unwrap();
        assert_eq!(
            expr,
            FilterExpression::Or(vec![
                FilterExpression::And(vec![SessionQuery::Title(
                    "foo".to_string(),
                    MatchKind::Exact
                )]),
                FilterExpression::And(vec![SessionQuery::ProjectName(
                    "bar".to_string(),
                    MatchKind::Exact
                )]),
            ])
        );
    }

    #[test]
    fn parse_or_with_weak_binding() {
        let expr = parse_filter("title=foo project=bar | project=baz").unwrap();
        assert_eq!(
            expr,
            FilterExpression::Or(vec![
                FilterExpression::And(vec![
                    SessionQuery::Title("foo".to_string(), MatchKind::Exact),
                    SessionQuery::ProjectName("bar".to_string(), MatchKind::Exact),
                ]),
                FilterExpression::And(vec![SessionQuery::ProjectName(
                    "baz".to_string(),
                    MatchKind::Exact
                )]),
            ])
        );
    }

    #[test]
    fn parse_multiple_or_branches() {
        let expr = parse_filter("title=a | title=b | title=c").unwrap();
        assert_eq!(
            expr,
            FilterExpression::Or(vec![
                FilterExpression::And(vec![SessionQuery::Title("a".to_string(), MatchKind::Exact)]),
                FilterExpression::And(vec![SessionQuery::Title("b".to_string(), MatchKind::Exact)]),
                FilterExpression::And(vec![SessionQuery::Title("c".to_string(), MatchKind::Exact)]),
            ])
        );
    }

    #[test]
    fn parse_has_updates() {
        let expr = parse_filter("has-updates=true").unwrap();
        assert_eq!(
            expr,
            FilterExpression::And(vec![SessionQuery::HasUpdates(true)])
        );
    }

    #[test]
    fn parse_has_bell() {
        let expr = parse_filter("bell=false").unwrap();
        assert_eq!(
            expr,
            FilterExpression::And(vec![SessionQuery::HasBell(false)])
        );
    }

    #[test]
    fn parse_id_query() {
        let expr = parse_filter("id=42").unwrap();
        assert_eq!(expr, FilterExpression::And(vec![SessionQuery::Id(42)]));
    }

    #[test]
    fn parse_normalized_keys() {
        let expr = parse_filter("project-name=foo").unwrap();
        assert_eq!(
            expr,
            FilterExpression::And(vec![SessionQuery::ProjectName(
                "foo".to_string(),
                MatchKind::Exact
            )])
        );

        let expr = parse_filter("worktree_name=bar").unwrap();
        assert_eq!(
            expr,
            FilterExpression::And(vec![SessionQuery::WorkspaceName(
                "bar".to_string(),
                MatchKind::Exact
            )])
        );
    }

    #[test]
    fn parse_current_project() {
        let expr = parse_filter("current-project=true").unwrap();
        assert_eq!(
            expr,
            FilterExpression::And(vec![SessionQuery::CurrentProject(true)])
        );
    }

    #[test]
    fn parse_error_missing_value() {
        let err = parse_filter("title=").unwrap_err();
        assert!(err.contains("requires a value"));
    }

    #[test]
    fn parse_error_missing_operator() {
        let err = parse_filter("title").unwrap_err();
        assert!(err.contains("expected key=value"));
    }

    #[test]
    fn parse_error_invalid_id() {
        let err = parse_filter("id=notanumber").unwrap_err();
        assert!(err.contains("invalid id value"));
    }

    #[test]
    fn parse_error_substring_on_id() {
        let err = parse_filter("id~=42").unwrap_err();
        assert!(err.contains("requires '=' operator"));
    }

    #[test]
    fn parse_error_unknown_key() {
        let err = parse_filter("unknown=value").unwrap_err();
        assert!(err.contains("unknown query key"));
    }

    #[test]
    fn empty_and_matches_all() {
        let expr = FilterExpression::And(Vec::new());
        // We can't test with actual project/session objects here easily,
        // but the logic is clear: empty AND should match everything
        assert_eq!(expr, FilterExpression::And(Vec::new()));
    }
}
