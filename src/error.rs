
pub enum ErrorLevel {
    Fatal,
    Error,
    Warn,
    Info,
    Note,
    Help,
    Cancelled
}

pub fn append_error_level(buf: &mut String, level: ErrorLevel) {
    match level {
        Error_Level::Fatal => {
            buf.push_str(escape_color_serious);
            buf.push_str("fatal");
        }
        Error_Level::Error => {
            buf.push_str(escape_color_bright_red);
            buf.push_str("error");
        }
        Error_Level::Warn => {
            buf.push_str(escape_color_bright_yellow);
            buf.push_str("warn");
        }   
        Error_Level::Info => {
            buf.push_str(escape_color_bright_blue);
            buf.push_str("info");
        }
        Error_Level::Note => {
            buf.push_str(escape_color_bright_blue);
            buf.push_str("note");
        }
        Error_Level::Help => {
            buf.push_str(escape_color_bright_blue);
            buf.push_str("help");
        }
        Error_Level::Cancelled => return
    }
    buf.push_str(escape_color_reset);
    buf.push(' ');
}

pub fn append_error_location(buf: &mut String, filepath: &str, line_number: usize, column_number: usize) {
    buf.push_str(filepath);
    buf.push(':');
    buf.push_str(line_number);
    buf.push(':');
    buf.push_str(column_number);
    buf.push(':');
    buf.push(' ');
}

pub fn append_annotated_code(
    buf: &mut String,
    source: &str,
    line_number: usize,
    column_number: isize,
    annotation_length: isize,
    label: &str
) {
    let d = line_number.log10().floor() + 1;
    for i in 0..=d {
        buf.push_str(' ');
    }
    buf.push_str(escape_color_blue);
    buf.push('|');
    buf.push_str(escape_color_reset);
    buf.push('\n');

    buf.push_str(line_number);
    buf.push(' ');
    buf.push_str(escape_color_blue);
    buf.push('|');
    buf.push_str(escape_color_reset);
    buf.push(' ');
    buf.push_str(source);
    if source.length == 0 || source[source.length - 1] != '\n' {
        buf.push('\n');
    }

    for i in 0..=d {
        buf.push(' ');
    }
    buf.push_str(escape_color_blue);
    buf.push('|');
    buf.push_str(escape_color_reset);
    for i in 0..=column_number {
        buf.push(' ');
    }
    buf.push_str(escape_color_red);
    buf.push('^');
    for i in 0..=annotation_length {
        buf.push('~');
    }
    buf.push(' ');
    buf.push_str(label);
    buf.push_str(escape_color_reset);
    buf.push('\n');
}

static escape_color_reset:          &'static str = "\033[0m";
static escape_color_bold:           &'static str = "\033[1m";
static escape_color_faint:          &'static str = "\033[2m";
static escape_color_italic:         &'static str = "\033[3m";
static escape_color_underline:      &'static str = "\033[4m";
static escape_color_black:          &'static str = "\033[30m";
static escape_color_red:            &'static str = "\033[31m";
static escape_color_green:          &'static str = "\033[32m";
static escape_color_yellow:         &'static str = "\033[33m";
static escape_color_blue:           &'static str = "\033[34m";
static escape_color_magenta:        &'static str = "\033[35m";
static escape_color_cyan:           &'static str = "\033[36m";
static escape_color_white:          &'static str = "\033[37m";
static escape_color_bg_black:       &'static str = "\033[40m";
static escape_color_bg_red:         &'static str = "\033[41m";
static escape_color_bg_green:       &'static str = "\033[42m";
static escape_color_bg_yellow:      &'static str = "\033[43m";
static escape_color_bg_blue:        &'static str = "\033[44m";
static escape_color_bg_magenta:     &'static str = "\033[45m";
static escape_color_bg_cyan:        &'static str = "\033[46m";
static escape_color_bg_white:       &'static str = "\033[47m";
static escape_color_bright_black:   &'static str = "\033[30;1m";
static escape_color_bright_red:     &'static str = "\033[31;1m";
static escape_color_bright_green:   &'static str = "\033[32;1m";
static escape_color_bright_yellow:  &'static str = "\033[33;1m";
static escape_color_bright_blue:    &'static str = "\033[34;1m";
static escape_color_bright_magenta: &'static str = "\033[35;1m";
static escape_color_bright_cyan:    &'static str = "\033[36;1m";
static escape_color_bright_white:   &'static str = "\033[37;1m";
static escape_color_serious:        &'static str = "\033[37;1m\033[41m";
