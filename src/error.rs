

use crate::{CodeArea, EmeraldSource, EmeraldCache, interpreter::{Globals}};
use ariadne::{Report, ReportKind, Label, Fmt, ReportBuilder};





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
    DuplicateFieldStructVariant {
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
    VectorMismatch {
        in_rot: bool,
        area: CodeArea,
    },
    DuplicateEnumVariant {
        variant_name: String,
        first_used: CodeArea,
        used_again: CodeArea,
    },
    DuplicateSelectorArg {
        arg_name: String,
        first_used: CodeArea,
        used_again: CodeArea,
    },
    NonexistentSelectorArg {
        arg_name: String,
        used: CodeArea,
    },
    TypeSelectorArg {
        used: CodeArea,
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
    ArgumentNotProvided {
        arg_name: String,
        arg_area: CodeArea,
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
    ContinueUsedOutside {
        continue_area: CodeArea,
        outer_area: CodeArea,
    },
    ReturnUsedOutsideProgram {
        return_area: CodeArea,
    },
    BreakUsedOutsideProgram {
        break_area: CodeArea,
    },
    ContinueUsedOutsideProgram {
        continue_area: CodeArea,
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
        type_str: String,
        val_area: CodeArea,
        area: CodeArea,
    },
    InstanceNonStruct {
        area: CodeArea,
    },
    InstanceNonEnum {
        area: CodeArea,
    },
    TypeArgCount {
        provided: usize,
        call_area: CodeArea,
    },
    IndexArgCount {
        provided: usize,
        expected: String,
        typ: String,
        val_area: CodeArea,
        index_area: CodeArea,
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
    NonexistentFile {
        path: String,
        area: CodeArea,
    },
    ErrorParsingImport {
        import_area: CodeArea,
        error: SyntaxError,
    },
    CantImportInEval {
        import_area: CodeArea,
    },
    EqualAssertionFailed {
        value1: String,
        value2: String,
        area1: CodeArea,
        area2: CodeArea,
    },
    DestructureTypeMismatch {
        tried: String,
        found: String,
        area1: CodeArea,
        area2: CodeArea,
    },
    DestructureLengthMismatch {
        for_type: String,
        expected: usize,
        found: usize,
        area1: CodeArea,
        area2: CodeArea,
    },
    DestructureNonExistentKeyField {
        for_type: String,
        what: String,
        name: String,
        area1: CodeArea,
        area2: CodeArea,
    },
    NonexistentVariant {
        variant_name: String,
        used: CodeArea,
        enum_def: CodeArea,
    },
    IncorrectVariantType {
        variant_name: String,
        expected: String,
        found: String,
        used: CodeArea,
        variant_def: CodeArea,
    },
    TupleVariantNotEnoughArguments {
        variant_name: String,
        expected: usize,
        found: usize,
        used: CodeArea,
        variant_def: CodeArea,
    },
    NoStructVariantField {
        field_name: String,
        variant_name: String,
        used: CodeArea,
        variant_def: CodeArea,
    },
    MissingStructVariantFields {
        fields: Vec<String>,
        variant_name: String,
        area: CodeArea,
        variant_def: CodeArea,
    },
    DestructureVariantMismatch {
        tried: String,
        found: String,
        area1: CodeArea,
        area2: CodeArea,
    },
    DestructureIncorrectVariantType {
        expected: String,
        found: String,
        expected_area: CodeArea,
        found_area: CodeArea,
    },
    DestructureOptionMismatch {
        tried: String,
        found: String,
        area1: CodeArea,
        area2: CodeArea,
    },
    CannotIter {
        typ: String,
        area: CodeArea,
        reason: String,
        // area2: CodeArea,
    },
    IncorrectSelectorArgType {
        arg_name: String,
        expected: String,
        value_area: CodeArea,
    },
    Throw {
        msg: String,
        area: CodeArea,
    },



    CustomError {
        msg: String,
        area: CodeArea,
        labels: Vec<(String, CodeArea)>,
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

#[derive(Debug)]
pub struct RainbowColorGenerator {
    h: f64,
    s: f64,
    v: f64,
    shift: f64,
}

impl RainbowColorGenerator {
    pub fn new(h: f64, s: f64, v: f64, shift: f64) -> Self {
        Self { h, s, v, shift }
    }
    pub fn next(&mut self) -> ariadne::Color {

        // thanks wikipedia
        
        self.h = self.h.rem_euclid(360.0);

        let c = self.v * self.s;
        let h1 = self.h / 60.0;

        let x = c * (1.0 - (h1.rem_euclid(2.0) - 1.0).abs());
        
        let (r, g, b) =
            if 0.0 <= h1 && h1 < 1.0 {
                (c, x, 0.0)
            } else if 1.0 <= h1 && h1 < 2.0 {
                (x, c, 0.0)
            } else if 2.0 <= h1 && h1 < 3.0 {
                (0.0, c, x)
            } else if 3.0 <= h1 && h1 < 4.0 {
                (0.0, x, c)
            } else if 4.0 <= h1 && h1 < 5.0 {
                (x, 0.0, c)
            } else {
                (c, 0.0, x)
            };

        let m = self.v - c;

        self.h += self.shift;

        ariadne::Color::RGB(
            ( (r + m) * 255.0) as u8,
            ( (g + m) * 255.0) as u8,
            ( (b + m) * 255.0) as u8,
        )
    }
}

const ERROR_S: f64 = 0.4;
const ERROR_V: f64 = 1.0;


impl ErrorReport {
    pub fn print_error(&self, cache: EmeraldCache, _globals: &Globals) {

        let mut colors = RainbowColorGenerator::new(0.0, ERROR_S, ERROR_V, 60.0);


        let mut report: ReportBuilder<CodeArea> = Report::build(ReportKind::Error, self.source.clone(), self.source.range.0)
        .with_message(self.message.clone() + "\n");

        // for (i, t) in globals.import_trace.iter().enumerate() {
        //     report = report.with_label( Label::new(t.clone()).with_message(format!("Error when running this import")).with_color(colors.next()).with_order(-(i as i32) - 1) )
        // }

        for (c, s) in &self.labels {
            report = report.with_label( Label::new(c.clone()).with_message(s).with_color(colors.next()) )
        }
        
        // let mut colors = RainbowColorGenerator::new(270.0, 1.0, 0.75);
        // for (i, t) in globals.trace.iter().enumerate() {
        //     report = report.with_label( Label::new(t.clone()).with_message(format!("{}: Error comes from this function call", i + 1)).with_color(colors.next()) )
        // }


        if let Some(m) = &self.note {
            report = report.with_note(m)
        }
        report.finish().print(cache).unwrap();

    }
}


impl ToReport for SyntaxError {
    fn to_report(&self) -> ErrorReport {
        let mut colors = RainbowColorGenerator::new(120.0, ERROR_S, ERROR_V, 20.0);

        match self {
            SyntaxError::Expected {
                expected,
                found,
                area,
            } => ErrorReport {
                source: area.clone(),
                message: "Syntax error".to_string(),
                labels: vec![
                    (area.clone(), format!("Expected {}, found {}", expected.fg(colors.next()), found.fg(colors.next())))
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
                    (area.clone(), format!("'{}' used here", for_char.fg(colors.next())))
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
            SyntaxError::DuplicateFieldStructVariant {
                field_name,
                first_used,
                used_again,
            } => ErrorReport {
                source: used_again.clone(),
                message: format!("Duplicate field name '{}' in struct variant", field_name),
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
            SyntaxError::VectorMismatch {
                in_rot,
                area,
            } => ErrorReport {
                source: area.clone(),
                message: (if *in_rot {"Cannot use caret coords in rotation"} else {"Cannot mix caret coords and relative/absolute coords"}).to_string(),
                labels: vec![
                    (area.clone(), format!("Coord used here"))
                ],
                note: None,
            },
            SyntaxError::DuplicateEnumVariant {
                variant_name,
                first_used,
                used_again,
            } => ErrorReport {
                source: used_again.clone(),
                message: format!("Duplicate variant name '{}' in enum definition", variant_name),
                labels: vec![
                    (first_used.clone(), format!("Variant name first used here")),
                    (used_again.clone(), format!("Used again here")),
                ],
                note: None,
            },
            SyntaxError::DuplicateSelectorArg {
                arg_name,
                first_used,
                used_again,
            } => ErrorReport {
                source: used_again.clone(),
                message: format!("The target selector argument '{}' can only be used once", arg_name),
                labels: vec![
                    (first_used.clone(), format!("Argument first used here")),
                    (used_again.clone(), format!("Used again here")),
                ],
                note: None,
            },
            SyntaxError::NonexistentSelectorArg {
                arg_name,
                used,
            } => ErrorReport {
                source: used.clone(),
                message: format!("Target selector argument '{}' doesn't exist", arg_name),
                labels: vec![
                    (used.clone(), format!("Argument used here")),
                ],
                note: None,
            },
            SyntaxError::TypeSelectorArg {
                used,
            } => ErrorReport {
                source: used.clone(),
                message: format!("Target selector argument 'type' can only be used in @e and @s"),
                labels: vec![
                    (used.clone(), format!("Argument used here")),
                ],
                note: None,
            },
        }
    }
}

impl ToReport for RuntimeError {
    fn to_report(&self) -> ErrorReport {
        let mut colors = RainbowColorGenerator::new(240.0, ERROR_S, ERROR_V, 20.0);

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
                    new_vec.push( (area.clone(), format!("Expected {}, found {}", expected.fg(colors.next()), found.fg(colors.next()))) );
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
                    (area.clone(), format!("'{}' used here", var_name.fg(colors.next())))
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
                    (header_area.clone(), format!("Function defined to take {} arguments here", takes.fg(colors.next()))),
                    (call_area.clone(), format!("{} arguments were provided here", provided.fg(colors.next())))
                ],
                note: None,
            },
            RuntimeError::ArgumentNotProvided {
                arg_area,
                arg_name,
                call_area,
            } => ErrorReport {
                source: call_area.clone(),
                message: format!("Required argument '{}' was not provided", arg_name),
                labels: vec![
                    (arg_area.clone(), format!("Argument '{}' defined here", arg_name.fg(colors.next()))),
                    (call_area.clone(), format!("Argument was not provided here"))
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
                    (area.clone(), format!("Index provided is {} but length is {}", index.fg(colors.next()), length.fg(colors.next())))
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
                    (area.clone(), format!("Key '{}' was used here", key.fg(colors.next())))
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
            RuntimeError::ContinueUsedOutside {
                continue_area,
                outer_area,
            } => ErrorReport {
                source: continue_area.clone(),
                message: format!("Continue used outside of loop"),
                labels: vec![
                    (continue_area.clone(), format!("Continue was used here")),
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
            RuntimeError::ContinueUsedOutsideProgram {
                continue_area,
            } => ErrorReport {
                source: continue_area.clone(),
                message: format!("Continue reached outside program"),
                labels: vec![
                    (continue_area.clone(), format!("Continue was used here")),
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
                    (area.clone(), format!("Couldn't convert {} to {}", type1.fg(colors.next()), type2.fg(colors.next()))),
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
                type_str,
                val_area,
                area,
            } => ErrorReport {
                source: area.clone(),
                message: format!("Field '{}' does not exist on type {}", field, type_str),
                labels: vec![
                    (area.clone(), format!("Field '{}' was used here", field.fg(colors.next()))),
                    (val_area.clone(), format!("This is of type {}", type_str.fg(colors.next())))
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
            RuntimeError::InstanceNonEnum {
                area,
            } => ErrorReport {
                source: area.clone(),
                message: format!("Can't instance variant of non-enum"),
                labels: vec![
                    (area.clone(), format!("This isn't an enum type"))
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
                    (call_area.clone(), format!("{} arguments were provided here", provided.fg(colors.next())))
                ],
                note: None,
            },
            RuntimeError::IndexArgCount {
                provided,
                index_area,
                expected,
                typ,
                val_area
            } => ErrorReport {
                source: index_area.clone(),
                message: format!("{} arguments provided, but indexing {} takes {}", provided, typ, expected),
                labels: vec![
                    (index_area.clone(), format!("{} arguments were provided here", provided.fg(colors.next()))),
                    (val_area.clone(), format!("This {} takes {} arguments", typ.fg(colors.next()), expected.fg(colors.next())))
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
                    (pattern_area.clone(), format!("Pattern defined as {} here", pattern.fg(colors.next()))),
                    (type_area.clone(), format!("This {} is not {}", typ.fg(colors.next()), pattern.fg(colors.next()))),
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
                    (area.clone(), format!("Member '{}' was used here", assoc.fg(colors.next())))
                ],
                note: None,
            },
            RuntimeError::NonexistentFile {
                path,
                area,
            } => ErrorReport {
                source: area.clone(),
                message: format!("File at path '{}' was not found", path),
                labels: vec![
                    (area.clone(), format!("Import used here"))
                ],
                note: None,
            },
            RuntimeError::ErrorParsingImport {
                import_area,
                error,
            } => {
                let mut labels = error.to_report().labels;
                for i in &labels {
                    println!("{:#?}", i.0);
                }
                labels.insert(0, (import_area.clone(), format!("Import used here")));

                ErrorReport {
                    source: import_area.clone(),
                    message: format!("Error parsing this import"),
                    labels,
                    note: None,
                }
            },
            RuntimeError::CantImportInEval {
                import_area,
            } => ErrorReport {
                source: import_area.clone(),
                message: format!("Can't import in eval"),
                labels: vec![
                    (import_area.clone(), format!("Import used here"))
                ],
                note: None,
            },
            RuntimeError::EqualAssertionFailed {
                value1,
                value2,
                area1,
                area2,
            } => ErrorReport {
                source: area1.clone(),
                message: format!("Equality assertion failed"),
                labels: vec![
                    (area1.clone(), format!("Found {} here", value1.fg(colors.next()))),
                    (area2.clone(), format!("Found {} here", value2.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::DestructureTypeMismatch {
                tried,
                found,
                area1,
                area2,
            } => ErrorReport {
                source: area2.clone(),
                message: format!("Cannot destructure {} into {}", found, tried),
                labels: vec![
                    (area1.clone(), format!("Tried to destructure {} here", tried.fg(colors.next()))),
                    (area2.clone(), format!("Found {} here", found.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::DestructureOptionMismatch {
                tried,
                found,
                area1,
                area2,
            } => ErrorReport {
                source: area2.clone(),
                message: format!("Cannot destructure option {} into option {}", found, tried),
                labels: vec![
                    (area1.clone(), format!("Tried to destructure option {} here", tried.fg(colors.next()))),
                    (area2.clone(), format!("Found option {} here", found.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::DestructureLengthMismatch {
                for_type,
                expected,
                found,
                area1,
                area2,
            } => ErrorReport {
                source: area2.clone(),
                message: format!("Unequal elements in {} destructure", for_type),
                labels: vec![
                    (area1.clone(), format!("Tried to destructure {} with {} elements here", for_type.fg(colors.next()), expected.fg(colors.next()))),
                    (area2.clone(), format!("Found {} elements here", found.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::DestructureNonExistentKeyField {
                what,
                for_type,
                name,
                area1,
                area2,
            } => ErrorReport {
                source: area2.clone(),
                message: format!("Nonexistent {} in {} destructure", what, for_type),
                labels: vec![
                    (area1.clone(), format!("Tried to destructure into {} {} here", what, name.fg(colors.next()))),
                    (area2.clone(), format!("{} {} not found here", what, name.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::NonexistentVariant {
                variant_name,
                used,
                enum_def,
            } => ErrorReport {
                source: used.clone(),
                message: format!("Nonexistent enum variant {}", variant_name),
                labels: vec![
                    (used.clone(), format!("Variant name used here")),
                    (enum_def.clone(), format!("Enum defined here")),
                ],
                note: None,
            },
            RuntimeError::IncorrectVariantType {
                variant_name,
                expected,
                found,
                used,
                variant_def,
            } => ErrorReport {
                source: used.clone(),
                message: format!("Wrong enum variant type"),
                labels: vec![
                    (used.clone(), format!("Expected {} variant, found {} variant", expected.fg(colors.next()), found.fg(colors.next()))),
                    (variant_def.clone(), format!("Variant {} defined as {} type here", variant_name.fg(colors.next()), expected.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::TupleVariantNotEnoughArguments {
                variant_name,
                expected,
                found,
                used,
                variant_def,
            } => ErrorReport {
                source: used.clone(),
                message: format!("{} arguments provided, but tuple variant expects {}", found, expected),
                labels: vec![
                    (used.clone(), format!("Found {} arguments here", found.fg(colors.next()))),
                    (variant_def.clone(), format!("Tuple variant {} defined as taking {} arguments here", variant_name.fg(colors.next()), expected.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::NoStructVariantField {
                variant_name,
                field_name,
                used,
                variant_def,
            } => ErrorReport {
                source: used.clone(),
                message: format!("Nonexistent struct variant field {}", field_name),
                labels: vec![
                    (used.clone(), format!("Field name used here")),
                    (variant_def.clone(), format!("Variant {} defined here", variant_name.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::MissingStructVariantFields {
                fields,
                variant_name,
                area,
                variant_def,
            } => ErrorReport {
                source: area.clone(),
                message: format!("Missing struct variant fields"),
                labels: vec![
                    (area.clone(), format!("Missing struct fields {} here", fields.iter().map(
                        |f| format!("{}", f.fg(colors.next()))
                    ).collect::<Vec<String>>().join(", "))),
                    (variant_def.clone(), format!("Struct variant {} defined here", variant_name.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::DestructureVariantMismatch {
                tried,
                found,
                area1,
                area2,
            } => ErrorReport {
                source: area2.clone(),
                message: format!("Cannot destructure enum variant {} into variant {}", found, tried),
                labels: vec![
                    (area1.clone(), format!("Tried to destructure enum variant {} here", tried.fg(colors.next()))),
                    (area2.clone(), format!("Found variant {} here", found.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::DestructureIncorrectVariantType {
                expected,
                found,
                expected_area,
                found_area,
            } => ErrorReport {
                source: found_area.clone(),
                message: format!("Wrong enum variant type to destructure"),
                labels: vec![
                    (expected_area.clone(), format!("Tried to destructure into {} variant here", expected.fg(colors.next()))),
                    (found_area.clone(), format!("Found {} variant here", found.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::CannotIter {
                typ,
                area,
                reason,
            } => ErrorReport {
                source: area.clone(),
                message: format!("Cannot iterate over this {}", typ),
                labels: vec![
                    (area.clone(), reason.to_string()),
                ],
                note: None,
            },
            RuntimeError::IncorrectSelectorArgType {
                expected,
                value_area,
                arg_name,
            } => ErrorReport {
                source: value_area.clone(),
                message: format!("Incorrect selector argument type"),
                labels: vec![
                    (value_area.clone(), format!("Selector argument {} takes a {}", arg_name.fg(colors.next()), expected.fg(colors.next()))),
                ],
                note: None,
            },
            RuntimeError::Throw {
                msg,
                area
            } => ErrorReport {
                source: area.clone(),
                message: format!("{}", msg),
                labels: vec![
                    (area.clone(), format!("Thrown here")),
                ],
                note: None,
            },
            RuntimeError::CustomError {
                msg,
                labels,
                area,
            } => ErrorReport {
                source: area.clone(),
                message: format!("{}", msg),
                labels: labels.iter().map(
                    |(s, a)|
                    (a.clone(), s.clone())
                ).collect(),
                note: None,
            },
        }
    }
}


impl RuntimeError {
    pub fn is_destructure_error(&self) -> bool {
        match self {
            RuntimeError::EqualAssertionFailed { .. } |
            RuntimeError::DestructureTypeMismatch { .. } |
            RuntimeError::DestructureOptionMismatch { .. } |
            RuntimeError::DestructureVariantMismatch { .. } |
            RuntimeError::DestructureLengthMismatch { .. } |
            RuntimeError::DestructureIncorrectVariantType { .. } |
            RuntimeError::DestructureNonExistentKeyField { .. } => true,
            _ => false,
        }
    }
}



