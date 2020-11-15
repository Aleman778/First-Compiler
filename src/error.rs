
use crate::span::Span;
use crate::parser::ParseSpan;


enum Error_Level {
    Fatal,
    Error,
    Warn,
    Info,
    Note,
    Help,
    Cancelled
};

pub fn append_error_level(buf: &mut String, level: Error_Level) {
    match level {
        Error_Level::Fatal => {
            buf.push_str(escape_color_serious());
            buf.push_str("fatal");
        }
        Error_Level::Error => {
            buf.push_str(escape_color_bright_red());
            buf.push_str("error");
        }
        Error_Level::Warn => {
            buf.push_str(escape_color_bright_yellow());
            buf.push_str("warn");
        }   
        Error_Level::Info => {
            buf.push_str(escape_color_bright_blue());
            buf.push_str("info");
        }
        Error_Level::Note => {
            buf.push_str(escape_color_bright_blue());
            buf.push_str("note");
        }
        Error_Level::Help => {
            buf.push_str(escape_color_bright_blue());
            buf.push_str("help");
        }
        Error_Level::Cancelled => return;
    }
    buf.push_str(escape_color_reset());
    buf.push_str(' ');
}

pub fn append_error_location(buf: &mut String, filepath: String, line_number: usize, column_number: usize) {

}

pub fn append_annotated_code(
    buf: &mut String,
    source: String,
    line_number: usize,
    column_number: isize,
    annotation_length: isize,
    label: String
) {
    u32 d = floor(log10(line_number + 1) + 1);
    for i in 0..=d {
        buf.push_str(' ');
    }
    buf.push_str(escape_color_blue());
    buf.push_str('|');
    buf.push_str(escape_color_reset());
    buf.push_str('\n');

    buf.push_str(line_number);
    buf.push_str(' ');
    buf.push_str(escape_color_blue());
    buf.push_str('|');
    buf.push_str(escape_color_reset());
    buf.push_str(' ');
    buf.push_str(source);
    if source.length == 0 || source[source.length - 1] != '\n' {
        buf.push_str('\n');
    }

    for i in 0..=d {
        buf.push_str(' ');
    }
    buf.push_str(escape_color_blue());
    buf.push_str('|');
    buf.push_str(escape_color_reset());
    for i in 0..=column_number {
        buf.push_str(' ');
    }
    for (int i = 0; i < column_number + 1; i++) buf.push_str(' ');
    buf.push_str(escape_color_red());
    buf.push_str('^');
    for i in 0..=annotated_length {
        buf.push_str('~');
    }
    buf.push_str(' ');
    buf.push_str(label);
    buf.push_str(escape_color_reset());
    buf.push_str('\n');
}

pub fn escape_color_reset() {
    return "\033[0m";
}

pub fn escape_color_bold() {
    return "\033[1m";
}

pub fn escape_color_faint() {
    return "\033[2m";
}

pub fn escape_color_italic() {
    return "\033[3m";
}

pub fn escape_color_underline() {
    return "\033[4m";
}

pub fn escape_color_black() {
    return "\033[30m";
}

pub fn escape_color_red() {
    return "\033[31m";
}

pub fn escape_color_green() {
    return "\033[32m";
}

pub fn escape_color_yellow() {
    return "\033[33m";
}

pub fn escape_color_blue() {
    return "\033[34m";
}

pub fn escape_color_magenta() {
    return "\033[35m";
}

pub fn escape_color_cyan() {
    return "\033[36m";
}

pub fn escape_color_white() {
    return "\033[37m";
}

pub fn escape_color_bg_black() {
    return "\033[40m";
}

pub fn escape_color_bg_red() {
    return "\033[41m";
}

pub fn escape_color_bg_green() {
    return "\033[42m";
}

pub fn escape_color_bg_yellow() {
    return "\033[43m";
}

pub fn escape_color_bg_blue() {
    return "\033[44m";
}

pub fn escape_color_bg_magenta() {
    return "\033[45m";
}

pub fn escape_color_bg_cyan() {
    return "\033[46m";
}

pub fn escape_color_bg_white() {
    return "\033[47m";
}

pub fn escape_color_bright_black() {
    return "\033[30;1m";
}

pub fn escape_color_bright_red() {
    return "\033[31;1m";
}

pub fn escape_color_bright_green() {
    return "\033[32;1m";
}

pub fn escape_color_bright_yellow() {
    return "\033[33;1m";
}

pub fn escape_color_bright_blue() {
    return "\033[34;1m";
}

pub fn escape_color_bright_magenta() {
    return "\033[35;1m";
}

pub fn escape_color_bright_cyan() {
    return "\033[36;1m";
}

pub fn escape_color_bright_white() {
    return "\033[37;1m";
}

pub fn escape_color_serious() {
    return "\033[37;1m\033[41m";
}
