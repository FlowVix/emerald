

use crate::{CodeArea, EmeraldSource, EmeraldCache, interpreter::RunInfo};
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
    },
    DuplicateField {
        field_name: String,
        first_used: CodeArea,
        used_again: CodeArea,
    },
    DuplicateFieldImpl {
        field_name: String,
        first_used: CodeArea,
        used_again: CodeArea,
    },
    DuplicateKey {
        key_name: String,
        first_used: CodeArea,
        used_again: CodeArea,
    },
    SelfNotFirstArg {
        area: CodeArea,
    },
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
    },
    NonexistentKey {
        key: String,
        area: CodeArea,
    },
    BreakUsedOutside {
        break_area: CodeArea,
        outer_area: CodeArea,
    },
    ReturnUsedOutsideProgram {
        return_area: CodeArea,
    },
    BreakUsedOutsideProgram {
        break_area: CodeArea,
    },
    CannotConvert {
        type1: String,
        type2: String,
        area: CodeArea,
        area1: CodeArea,
        // area2: CodeArea,
    },
    NoStructField {
        field_name: String,
        used: CodeArea,
        struct_def: CodeArea,
    },
    MissingStructFields {
        fields: Vec<String>,
        area: CodeArea,
        struct_def: CodeArea,
    },
    NonexistentField {
        field: String,
        area: CodeArea,
    },
    InstanceNonStruct {
        area: CodeArea,
    },
    TypeArgCount {
        provided: usize,
        call_area: CodeArea,
    },
    PatternMismatch {
        typ: String,
        pattern: String,
        pattern_area: CodeArea,
        type_area: CodeArea,
        // area2: CodeArea,
    },
    NoAssociatedMember {
        assoc: String,
        area: CodeArea,
    },
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
        self.h += 20.0;
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
    pub fn print_error(&self, cache: EmeraldCache, info: &RunInfo) {

        let mut colors = RainbowColorGenerator::new(0.0, 1.0, 0.75);


        let mut report: ReportBuilder<CodeArea> = Report::build(ReportKind::Error, self.source.clone(), self.source.range.0)
        .with_message(self.message.clone());

        for (c, s) in &self.labels {
            report = report.with_label( Label::new(c.clone()).with_message(s).with_color(colors.next()) )
        }
        
        let mut colors = RainbowColorGenerator::new(270.0, 1.0, 0.75);
        for t in &info.trace {
            report = report.with_label( Label::new(t.clone()).with_message("Error comes from this function call").with_color(colors.next()) )
        }


        if let Some(m) = &self.note {
            report = report.with_note(m)
        }
        report.finish().print(cache);

    }
}


impl ToReport for SyntaxError {
    fn to_report(&self) -> ErrorReport {
        let mut colors = RainbowColorGenerator::new(90.0, 1.0, 0.75);

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
            SyntaxError::DuplicateField {
                field_name,
                first_used,
                used_again,
            } => ErrorReport {
                source: used_again.clone(),
                message: format!("Duplicate field name '{}' in struct", field_name),
                labels: vec![
                    (first_used.clone(), format!("Field name first used here")),
                    (used_again.clone(), format!("Used again here")),
                ],
                note: None,
            },
            SyntaxError::DuplicateFieldImpl {
                field_name,
                first_used,
                used_again,
            } => ErrorReport {
                source: used_again.clone(),
                message: format!("Duplicate field name '{}' in impl", field_name),
                labels: vec![
                    (first_used.clone(), format!("Field name first used here")),
                    (used_again.clone(), format!("Used again here")),
                ],
                note: None,
            },
            SyntaxError::DuplicateKey {
                key_name,
                first_used,
                used_again,
            } => ErrorReport {
                source: used_again.clone(),
                message: format!("Duplicate key '{}' in dict", key_name),
                labels: vec![
                    (first_used.clone(), format!("Key first used here")),
                    (used_again.clone(), format!("Used again here")),
                ],
                note: None,
            },
            SyntaxError::SelfNotFirstArg {
                area,
            } => ErrorReport {
                source: area.clone(),
                message: format!("'self' must be the first argument"),
                labels: vec![
                    (area.clone(), format!("Argument 'self' defined here"))
                ],
                note: None,
            },
        }
    }
}

impl ToReport for RuntimeError {
    fn to_report(&self) -> ErrorReport {
        let mut colors = RainbowColorGenerator::new(180.0, 1.0, 0.75);

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
                        (def_a.clone(), format!("Value defined as {} here", t.fg(colors.next())))
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
            RuntimeError::NonexistentKey {
                key,
                area,
            } => ErrorReport {
                source: area.clone(),
                message: format!("Key '{}' does not exist", key),
                labels: vec![
                    (area.clone(), format!("Key '{}' was used here", key.fg(a)))
                ],
                note: None,
            },
            RuntimeError::BreakUsedOutside {
                break_area,
                outer_area,
            } => ErrorReport {
                source: break_area.clone(),
                message: format!("Break used outside of loop"),
                labels: vec![
                    (break_area.clone(), format!("Break was used here")),
                    (outer_area.clone(), format!("Reached here")),
                ],
                note: None,
            },
            RuntimeError::ReturnUsedOutsideProgram {
                return_area,
            } => ErrorReport {
                source: return_area.clone(),
                message: format!("Return reached outside program"),
                labels: vec![
                    (return_area.clone(), format!("Return was used here")),
                ],
                note: None,
            },
            RuntimeError::BreakUsedOutsideProgram {
                break_area,
            } => ErrorReport {
                source: break_area.clone(),
                message: format!("Break reached outside program"),
                labels: vec![
                    (break_area.clone(), format!("Break was used here")),
                ],
                note: None,
            },
            RuntimeError::CannotConvert {
                type1,
                type2,
                area,
                area1,
                // area2
            } => ErrorReport {
                source: area.clone(),
                message: format!("Cannot convert {} to {}", type1, type2),
                labels: vec![
                    (area.clone(), format!("Couldn't convert {} to {}", type1.fg(a), type2.fg(b))),
                    (area1.clone(), format!("Value defined as {} here", type1.fg(colors.next()))),
                    // (area2.clone(), format!("Conversion type {} here", type2.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::NoStructField {
                field_name,
                used,
                struct_def
            } => ErrorReport {
                source: used.clone(),
                message: format!("Nonexistent struct field {}", field_name),
                labels: vec![
                    (used.clone(), format!("Field name used here")),
                    (struct_def.clone(), format!("Struct defined here")),
                ],
                note: None,
            },
            RuntimeError::MissingStructFields {
                fields,
                area,
                struct_def
            } => ErrorReport {
                source: area.clone(),
                message: format!("Missing struct fields"),
                labels: vec![
                    (area.clone(), format!("Missing struct fields {} here", fields.iter().map(
                        |f| format!("{}", f.fg(colors.next()))
                    ).collect::<Vec<String>>().join(", "))),
                    (struct_def.clone(), format!("Struct defined here")),
                ],
                note: None,
            },
            RuntimeError::NonexistentField {
                field,
                area,
            } => ErrorReport {
                source: area.clone(),
                message: format!("Field '{}' does not exist", field),
                labels: vec![
                    (area.clone(), format!("Field '{}' was used here", field.fg(a)))
                ],
                note: None,
            },
            RuntimeError::InstanceNonStruct {
                area,
            } => ErrorReport {
                source: area.clone(),
                message: format!("Can't instance non-struct"),
                labels: vec![
                    (area.clone(), format!("This isn't a struct type"))
                ],
                note: None,
            },
            RuntimeError::TypeArgCount {
                provided,
                call_area,
            } => ErrorReport {
                source: call_area.clone(),
                message: format!("{} arguments provided, but type conversion takes 1", provided),
                labels: vec![
                    (call_area.clone(), format!("{} arguments were provided here", provided.fg(b)))
                ],
                note: None,
            },
            RuntimeError::PatternMismatch {
                typ,
                type_area,
                pattern,
                pattern_area,
                // area2
            } => ErrorReport {
                source: type_area.clone(),
                message: format!("Pattern mismatch"),
                labels: vec![
                    (pattern_area.clone(), format!("Pattern defined as {} here", pattern.fg(a))),
                    (type_area.clone(), format!("Value defined as {} here", typ.fg(b))),
                    // (area2.clone(), format!("Conversion type {} here", type2.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::NoAssociatedMember {
                assoc,
                area,
            } => ErrorReport {
                source: area.clone(),
                message: format!("Associated member '{}' does not exist", assoc),
                labels: vec![
                    (area.clone(), format!("Member '{}' was used here", assoc.fg(a)))
                ],
                note: None,
            },
        }
    }
}


