//! Handles parsing the configuration file

use std::collections::HashMap;
use std::default::Default;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::num::NonZeroUsize;
use std::path::Path;
use std::str::FromStr;

use crate::config;
use crate::error::ReadlineError;

type ParserResult<T> = Result<T, InputrcParsingError>;

pub fn parse_config<P: AsRef<Path>, H: crate::Helper>(
    config_path: P,
) -> crate::Result<crate::Editor<H>> {
    let mut parser = Parser {
        state: ParserState::Toplevel,
        statements: vec![],
    };
    let config_file = BufReader::new(File::open(config_path)?);

    parser.parse(config_file)?;
    Ok(parser.execute()?)
}

#[derive(Debug)]
pub enum InputrcParsingError {
    UnfinishedConditional(NonZeroUsize),
    MissingEndIf,
    ExtraEndIf,
    DanglingElse,
    InvalidSyntax(String),
    BadEqualityOp(String),
    BadComparisonOp(String),
    BadSetVariableSyntax(String),
    InvalidStatement(String),
    InvalidConditional(String),
}

impl From<InputrcParsingError> for ReadlineError {
    fn from(err: InputrcParsingError) -> Self {
        ReadlineError::Inputrc(err)
    }
}

/// State machine for the inputrc parser.
#[derive(Eq, PartialEq, Debug)]
enum ParserState {
    /// Not in a conditional, just append the line to the main body.
    Toplevel,
    /// In the middle of parsing a conditional. When an endif is encountered a
    /// conditional is popped from the list, or we move back to Statement form.
    Conditional {
        previous: Vec<Conditional>,
        current: Conditional,
    },
}

impl Default for ParserState {
    fn default() -> ParserState {
        ParserState::Toplevel
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
enum Statement {
    SetVariable {
        variable: String,
        value: Option<String>,
    },
    Keybinding {
        keyname: String,
        function: String,
    },
    Conditional(Conditional),
}

#[derive(Eq, PartialEq, Debug, Clone)]
struct Conditional {
    if_: (Condition, Vec<Statement>),
    else_: Option<Vec<Statement>>,
}

#[derive(Eq, PartialEq, Debug, Clone)]
enum Condition {
    Mode(config::EditMode),
    Term(String),
    Version {
        op: ComparisonOp,
        major: usize,
        minor: Option<usize>,
    },
    Application(String),
    Variable {
        variable: String,
        op: EqualityOp,
        expected_value: String,
    },
}

#[derive(Eq, PartialEq, Debug, Clone)]
enum EqualityOp {
    Equals,
    NotEquals,
}

impl FromStr for EqualityOp {
    type Err = InputrcParsingError;

    fn from_str(s: &str) -> ParserResult<Self> {
        match s {
            "==" | "=" => Ok(EqualityOp::Equals),
            "!=" => Ok(EqualityOp::NotEquals),
            _ => Err(InputrcParsingError::BadEqualityOp(s.into())),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
enum ComparisonOp {
    Equals,
    NotEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
}

impl FromStr for ComparisonOp {
    type Err = InputrcParsingError;

    fn from_str(s: &str) -> ParserResult<Self> {
        match s {
            "=" | "==" => Ok(ComparisonOp::Equals),
            "!=" => Ok(ComparisonOp::NotEquals),
            "<" => Ok(ComparisonOp::LessThan),
            "<=" => Ok(ComparisonOp::LessThanEquals),
            ">" => Ok(ComparisonOp::GreaterThan),
            ">=" => Ok(ComparisonOp::GreaterThanEquals),
            _ => Err(InputrcParsingError::BadComparisonOp(s.into())),
        }
    }
}

#[derive(Default, Debug)]
struct Parser {
    state: ParserState,
    statements: Vec<Statement>,
}

impl Eq for Parser {}

impl PartialEq for Parser {
    fn eq(&self, other: &Parser) -> bool {
        self.state == other.state && self.statements == other.statements
    }
}

impl Parser {
    fn parse<R: BufRead>(&mut self, config_file: R) -> crate::Result<()> {
        for line in config_file.lines() {
            self.parse_line(line?.as_ref())?
        }
        Ok(())
    }

    fn parse_line(&mut self, line: &str) -> ParserResult<()> {
        let stripped = line.trim();
        if stripped.starts_with("#") || stripped.is_empty() {
            Ok(())
        } else if stripped.starts_with("$") {
            self.parse_conditional(stripped)
        } else if stripped.starts_with("set") {
            self.parse_set_variable(stripped)
        } else if stripped.contains(":") {
            self.parse_keybinding(stripped)
        } else {
            Err(InputrcParsingError::InvalidStatement(line.into()))
        }
    }

    fn parse_conditional(&mut self, line: &str) -> ParserResult<()> {
        let end_of_first_word = line.find(char::is_whitespace).unwrap_or(line.len());
        let (first_word, rest) = line.split_at(end_of_first_word);
        match first_word {
            "$if" => {
                let mut conditional = self.parse_if(rest.trim())?;
                match self.state {
                    ParserState::Toplevel => {
                        self.state = ParserState::Conditional {
                            previous: vec![],
                            current: conditional,
                        };
                        Ok(())
                    }
                    ParserState::Conditional {
                        ref mut previous,
                        ref mut current,
                    } => {
                        std::mem::swap(current, &mut conditional);
                        previous.push(conditional);
                        Ok(())
                    }
                }
            }
            "$else" => {
                match self.state {
                    ParserState::Toplevel => Err(InputrcParsingError::InvalidSyntax(line.into()))?,
                    ParserState::Conditional {
                        ref mut current, ..
                    } => match current.else_ {
                        None => current.else_ = Some(vec![]),
                        Some(_) => Err(InputrcParsingError::DanglingElse)?,
                    },
                }
                Ok(())
            }
            "$endif" => match self.state {
                ParserState::Toplevel => Err(InputrcParsingError::ExtraEndIf),
                ParserState::Conditional {
                    ref mut previous,
                    ref mut current,
                } => {
                    self.statements
                        .push(Statement::Conditional(current.clone()));
                    match previous.pop() {
                        None => self.state = ParserState::Toplevel,
                        Some(new_current) => *current = new_current,
                    }
                    Ok(())
                }
            },
            "$include" => unimplemented!("$include is not implemented"),
            _ => Err(InputrcParsingError::InvalidConditional(line.into())),
        }
    }

    fn parse_if(&mut self, rest: &str) -> ParserResult<Conditional> {
        let mut words = rest.split(char::is_whitespace);
        let parts = (words.next(), words.next(), words.next());
        if words.next().is_some() {
            return Err(InputrcParsingError::InvalidSyntax(rest.into()));
        }
        let if_expr = match parts {
            (Some(expr), _, _) if expr.starts_with("mode=") => {
                let value = expr
                    .split("mode=")
                    .skip(1)
                    .next()
                    .ok_or_else(|| InputrcParsingError::InvalidSyntax(expr.into()))?;
                match value {
                    "vi" => Condition::Mode(config::EditMode::Vi),
                    "emacs" => Condition::Mode(config::EditMode::Emacs),
                    _ => return Err(InputrcParsingError::InvalidSyntax(expr.into())),
                }
            }
            (Some(expr), _, _) if expr.starts_with("term=") => Condition::Term(
                expr.split("term=")
                    .skip(1)
                    .next()
                    .ok_or_else(|| InputrcParsingError::InvalidSyntax(expr.into()))?
                    .into(),
            ),
            (Some(first), Some(op), Some(version)) if first == "version" => {
                let mut chunk = version.split(".");
                let (major, minor) = (
                    chunk
                        .next()
                        .ok_or_else(|| InputrcParsingError::InvalidSyntax(version.into()))?,
                    chunk.next(),
                );
                Condition::Version {
                    op: op.parse()?,
                    major: major
                        .parse()
                        .map_err(|_| InputrcParsingError::InvalidSyntax(major.into()))?,
                    minor: minor
                        .map(|minor| minor.parse::<usize>().ok())
                        .ok_or_else(|| {
                            InputrcParsingError::InvalidSyntax(minor.unwrap_or("").into())
                        })?,
                }
            }
            (Some(variable), Some(op), Some(expected_value)) => Condition::Variable {
                variable: variable.into(),
                op: op.parse()?,
                expected_value: expected_value.into(),
            },
            (Some(application), _, _) => Condition::Application(application.into()),
            _ => return Err(InputrcParsingError::InvalidSyntax(rest.into())),
        };
        Ok(Conditional {
            if_: (if_expr, vec![]),
            else_: None,
        })
    }

    fn parse_set_variable(&mut self, line: &str) -> ParserResult<()> {
        let mut parts = line.split(char::is_whitespace).skip(1).take(2);
        let variable = parts
            .next()
            .ok_or_else(|| InputrcParsingError::BadSetVariableSyntax(line.into()))?
            .into();
        let value = parts.next().map(String::from);
        self.add_statement(Statement::SetVariable { variable, value });
        Ok(())
    }

    fn parse_keybinding(&mut self, line: &str) -> ParserResult<()> {
        let mut parts = line.split(":");
        let keyname = parts
            .next()
            .ok_or_else(|| InputrcParsingError::InvalidSyntax(line.into()))?
            .into();
        let function = parts
            .next()
            .ok_or_else(|| InputrcParsingError::InvalidSyntax(line.into()))?
            .into();
        self.add_statement(Statement::Keybinding { keyname, function });
        Ok(())
    }

    fn add_statement(&mut self, statement: Statement) {
        match self.state {
            ParserState::Toplevel => self.statements.push(statement),
            ParserState::Conditional {
                ref mut current, ..
            } => match current {
                Conditional {
                    if_: (_, statements),
                    else_: None,
                }
                | Conditional {
                    else_: Some(statements),
                    ..
                } => statements.push(statement),
            },
        }
    }

    fn execute<H: crate::Helper>(mut self) -> ParserResult<crate::Editor<H>> {
        if let ParserState::Conditional { .. } = self.state {
            return Err(InputrcParsingError::MissingEndIf);
        }
        let mut config = crate::Config::default();
        self.statements.reverse();
        let mut environment = HashMap::new();
        while let Some(statement) = self.statements.pop() {
            match statement {
                Statement::SetVariable { variable, value } => {
                    environment.insert(variable.clone(), value.unwrap_or_else(String::new));
                    match variable.as_str() {
                        "bell-style" => unimplemented!(),
                        "bind-tty-special-chars" => unimplemented!(),
                        "blink-matching-paren" => unimplemented!(),
                        "colored-completion-prefix" => unimplemented!(),
                        "colored-stats" => unimplemented!(),
                        "comment-begin" => unimplemented!(),
                        "completion-display-width" => unimplemented!(),
                        "completion-ignore-case" => unimplemented!(),
                        "completion-map-case" => unimplemented!(),
                        "completion-prefix-display-length" => unimplemented!(),
                        "completion-query-items" => unimplemented!(),
                        "convert-meta" => unimplemented!(),
                        "display-completion" => unimplemented!(),
                        "echo-control-characters" => unimplemented!(),
                        "editing-mode" => unimplemented!(),
                        "emacs-mode-string" => unimplemented!(),
                        "enable-bracketed-paste" => unimplemented!(),
                        "enable-keypad" => unimplemented!(),
                        "expand-tilde" => unimplemented!(),
                        "history-preserve-point" => unimplemented!(),
                        "history-size" => unimplemented!(),
                        "horizontal-scroll-mode" => unimplemented!(),
                        "input-meta" => unimplemented!(),
                        "isearch-terminators" => unimplemented!(),
                        "keymap" => unimplemented!(),
                        "keyseq-timeout" => unimplemented!(),
                        "mark-directories" => unimplemented!(),
                        "mark-modified-lines" => unimplemented!(),
                        "mark-symlinked-directories" => unimplemented!(),
                        "match-hidden-files" => unimplemented!(),
                        "menu-complete-display-prefix" => unimplemented!(),
                        "output-meta" => unimplemented!(),
                        "page-completions" => unimplemented!(),
                        "print-completions-horizontally" => unimplemented!(),
                        "revert-all-at-newline" => unimplemented!(),
                        "show-all-if-ambiguous" => unimplemented!(),
                        "show-all-if-unmodified" => unimplemented!(),
                        "show-mode-in-prompt" => unimplemented!(),
                        "skip-completed-text" => unimplemented!(),
                        "vi-cmd-mode-string" => unimplemented!(),
                        "vi-ins-mode-string" => unimplemented!(),
                        "visible-stats" => unimplemented!(),
                        _ => {}
                    }
                }
                Statement::Conditional(Conditional {
                    if_: (conditional, if_),
                    else_,
                }) => {
                    let matches = match conditional {
                        Condition::Mode(mode) => config.edit_mode == mode,
                        Condition::Term(_) => unimplemented!(),
                        Condition::Version { op, major, minor } => {
                            let actual_major: usize = env!("CARGO_PKG_VERSION_MAJOR")
                                .parse()
                                .expect("Major version was not a number");
                            let actual_minor: usize = env!("CARGO_PKG_VERSION_MINOR")
                                .parse()
                                .expect("Minor version was not a number");
                            let minor = minor.unwrap_or(0);
                            match op {
                                ComparisonOp::Equals => {
                                    actual_major == major && actual_minor == minor
                                }
                                ComparisonOp::NotEquals => {
                                    actual_major != major && actual_minor != minor
                                }
                                ComparisonOp::LessThan => {
                                    actual_major < major
                                        || (actual_major == major && actual_minor < minor)
                                }
                                ComparisonOp::LessThanEquals => {
                                    actual_major < major
                                        || (actual_major == major && actual_minor <= minor)
                                }
                                ComparisonOp::GreaterThan => {
                                    actual_major > major
                                        || (actual_major == major && actual_minor > minor)
                                }
                                ComparisonOp::GreaterThanEquals => {
                                    actual_major > major
                                        || (actual_major == major && actual_minor >= minor)
                                }
                            }
                        }
                        Condition::Application(application) => unimplemented!(),
                        Condition::Variable {
                            variable,
                            op,
                            expected_value,
                        } => {
                            let actual_value =
                                environment.get(&variable).map(|s| s.as_ref()).unwrap_or("");
                            match op {
                                EqualityOp::Equals => actual_value == &expected_value,
                                EqualityOp::NotEquals => actual_value != &expected_value,
                            }
                        }
                    };
                    if matches {
                        self.statements.extend(if_)
                    } else {
                        self.statements.extend(else_.unwrap_or(vec![]))
                    }
                }
                Statement::Keybinding { keyname, function } => unimplemented!(),
            }
        }
        panic!()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    static SIMPLE_CONFIG: &str = "
set editing-mode vi
$if mode=vi

set keymap vi-command
# these are for vi-command mode
 Control-l: clear-screen
#
 set keymap vi-insert
# # these are for vi-insert mode
 Control-l: clear-screen
$endif
";

    #[test]
    fn parse_simple() {
        let mut parser = Parser::default();

        let expected = Parser {
            state: ParserState::Toplevel,
            statements: vec![
                Statement::SetVariable {
                    variable: "editing-mode".into(),
                    value: Some("vi".into()),
                },
                Statement::Conditional(Conditional {
                    if_: (
                        Condition::Mode(config::EditMode::Vi),
                        vec![
                            Statement::SetVariable {
                                variable: "keymap".into(),
                                value: Some("vi-command".into()),
                            },
                            Statement::Keybinding {
                                keyname: "Control-l".into(),
                                function: " clear-screen".into(),
                            },
                            Statement::SetVariable {
                                variable: "keymap".into(),
                                value: Some("vi-insert".into()),
                            },
                            Statement::Keybinding {
                                keyname: "Control-l".into(),
                                function: " clear-screen".into(),
                            },
                        ],
                    ),
                    else_: None,
                }),
            ],
        };
        parser.parse(Cursor::new(SIMPLE_CONFIG.as_bytes())).unwrap();
        assert_eq!(parser, expected);
    }

    #[test]
    fn execute_simple() {
        let mut parser = Parser::default();
        parser.parse(Cursor::new(SIMPLE_CONFIG.as_bytes())).unwrap();
        let editor = parser.execute::<()>().unwrap();
        let bindings = editor.custom_bindings.read().unwrap();
        assert_eq!(
            bindings.get(&crate::KeyPress::Ctrl('l')),
            Some(&crate::Cmd::ClearScreen)
        );
    }
}
