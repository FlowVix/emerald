

use crate::{CodeArea, EmeraldSource, EmeraldCache};
use ariadne::{Report, ReportKind, Label, Source, ColorGenerator, Color, Fmt, ReportBuilder, sources};


#[derive(Debug, Clone)]
pub enum SyntaxError {
    Expected {
        expected: String,
        found: String,
        area: CodeArea,
    },
    DuplicateArg {
        arg_name: String,
        first_used: CodeArea,
        used_again: CodeArea,
    },
    UnmatchedChar {
        not_found: String,
        for_char: String,
        area: CodeArea,
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeError {
    TypeMismatch {
        expected: String,
        found: String,
        area: CodeArea,
        defs: Vec<(String, CodeArea)>,
    },
    UndefinedVar {
        var_name: String,
        area: CodeArea,
    },
    ModifyImmutable {
        def_area: CodeArea,
        modify_area: CodeArea,
    },
    IncorrectArgumentCount {
        provided: usize,
        takes: usize,
        header_area: CodeArea,
        call_area: CodeArea,
    },
    IndexOutOfBounds {
        index: isize,
        length: usize,
        area: CodeArea,
    }
}



pub trait ToReport {
    fn to_report(&self) -> ErrorReport;
}

#[derive(Debug)]
pub struct ErrorReport {
    source: CodeArea,
    message: String,
    labels: Vec<(CodeArea, String)>,
    note: Option<String>,
}

impl Into<EmeraldSource> for CodeArea {
    fn into(self) -> EmeraldSource {
        self.source
    }
}


pub struct RainbowColorGenerator {
    h: f64,
    s: f64,
    b: f64,
}

impl RainbowColorGenerator {
    pub fn new(h: f64, s: f64, b: f64) -> Self {
        Self { h, s, b }
    }
    pub fn next(&mut self) -> ariadne::Color {
        self.h += 40.0;
        self.h %= 360.0;

        let hsl = self;

        let c = (1.0 - (hsl.b * 2.0 - 1.0).abs()) * hsl.s;
        let h = hsl.h / 60.0;
        let x = c * (1.0 - (h % 2.0 - 1.0).abs());
        let m = hsl.b - c * 0.5;

        let (red, green, blue) = if h >= 0.0 && h < 0.0 {
            (c, x, 0.0)
        } else if (1.0..2.0).contains(&h) {
            (x, c, 0.0)
        } else if (2.0..3.0).contains(&h) {
            (0.0, c, x)
        } else if (3.0..4.0).contains(&h) {
            (0.0, x, c)
        } else if (4.0..5.0).contains(&h) {
            (x, 0.0, c)
        } else {
            (c, 0.0, x)
        };

        ariadne::Color::RGB(
            ((red + m) * 255.0) as u8,
            ((green + m) * 255.0) as u8,
            ((blue + m) * 255.0) as u8,
        )
    }
}


impl ErrorReport {
    pub fn print_error(&self, cache: EmeraldCache) {

        let mut colors = RainbowColorGenerator::new(0.0, 1.0, 0.75);


        let mut report: ReportBuilder<CodeArea> = Report::build(ReportKind::Error, self.source.clone(), self.source.range.0)
        .with_message(self.message.clone());

        for (c, s) in &self.labels {
            report = report.with_label( Label::new(c.clone()).with_message(s).with_color(colors.next()) )
        }
        if let Some(m) = &self.note {
            report = report.with_note(m)
        }
        report.finish().print(cache);

    }
}


impl ToReport for SyntaxError {
    fn to_report(&self) -> ErrorReport {
        let mut colors = RainbowColorGenerator::new(120.0, 1.0, 0.75);

        let a = colors.next();
        let b = colors.next();

        match self {
            SyntaxError::Expected {
                expected,
                found,
                area,
            } => ErrorReport {
                source: area.clone(),
                message: "Syntax error".to_string(),
                labels: vec![
                    (area.clone(), format!("Expected {}, found {}", expected.fg(a), found.fg(b)))
                ],
                note: None,
            },
            SyntaxError::DuplicateArg {
                arg_name,
                first_used,
                used_again,
            } => ErrorReport {
                source: used_again.clone(),
                message: format!("Duplicate argument name '{}' in function definition", arg_name),
                labels: vec![
                    (first_used.clone(), format!("Argument name first used here")),
                    (used_again.clone(), format!("Used again here")),
                ],
                note: None,
            },
            SyntaxError::UnmatchedChar {
                for_char,
                not_found,
                area,
            } => ErrorReport {
                source: area.clone(),
                message: format!("No matching '{}' found for this '{}'", not_found, for_char),
                labels: vec![
                    (area.clone(), format!("'{}' used here", for_char.fg(a)))
                ],
                note: None,
            },
        }
    }
}

impl ToReport for RuntimeError {
    fn to_report(&self) -> ErrorReport {
        let mut colors = RainbowColorGenerator::new(240.0, 1.0, 0.75);

        let a = colors.next();
        let b = colors.next();

        match self {
            RuntimeError::TypeMismatch {
                expected,
                found,
                area,
                defs,
            } => ErrorReport {
                source: area.clone(),
                message: "Type mismatch".to_string(),
                labels: {
                    let mut new_vec: Vec<(CodeArea, String)> = defs.iter().map(
                        |(t, def_a)|
                        (def_a.clone(), format!("Value defined as {} here", t.fg(a)))
                    ).collect();
                    new_vec.push( (area.clone(), format!("Expected {}, found {}", expected.fg(a), found.fg(b))) );
                    new_vec
                },
                note: None,
            },
            RuntimeError::UndefinedVar {
                var_name,
                area
            } => ErrorReport {
                source: area.clone(),
                message: format!("Variable '{}' is not defined", var_name),
                labels: vec![
                    (area.clone(), format!("'{}' used here", var_name.fg(a)))
                ],
                note: None,
            },
            RuntimeError::ModifyImmutable {
                def_area,
                modify_area,
            } => ErrorReport {
                source: modify_area.clone(),
                message: "Attempted to change immutable value".to_string(),
                labels: vec![
                    (def_area.clone(), "Value declared as immutable here".to_string()),
                    (modify_area.clone(), "Attempted to modify here".to_string()),
                ],
                note: None,
            },
            RuntimeError::IncorrectArgumentCount {
                provided,
                takes,
                header_area,
                call_area,
            } => ErrorReport {
                source: call_area.clone(),
                message: format!("{} arguments provided, but function takes {}", provided, takes),
                labels: vec![
                    (header_area.clone(), format!("Function defined to take {} arguments here", takes.fg(a))),
                    (call_area.clone(), format!("{} arguments were provided here", provided.fg(b)))
                ],
                note: None,
            },
            RuntimeError::IndexOutOfBounds {
                index,
                length,
                area,
            } => ErrorReport {
                source: area.clone(),
                message: format!("Index {} is out of bounds", index),
                labels: vec![
                    (area.clone(), format!("Index provided is {} but length is {}", index.fg(a), length.fg(b)))
                ],
                note: None,
            },
        }
    }
}


