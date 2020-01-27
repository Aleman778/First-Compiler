
/***************************************************************************
 * Some common compiler utilities.
 ***************************************************************************/


use std::io;
use std::io::Write;
use termcolor::{
    StandardStream, WriteColor, BufferWriter,
    Buffer, Ansi, ColorChoice, ColorSpec, Color
};
use crate::sqrrlc::error::{
    Level,
    snippet::Style,
};


/**
 * The color config defines if color should
 * be used when writing to e.g. terminal.
 */
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ColorConfig {
    Auto,
    Always,
    Never,
}


/**
 * Destination enum defines different ways of writing
 * text both colored and non colored options.
 */
pub enum Destination {
    /// Writes directly to terminal using stdout/stderr.
    Terminal(StandardStream),
    /// Writes first to buffer that can then write to terminal.
    Buffered(BufferWriter),
    /// Write to raw object with write and send traits, set bool true for colors.
    Raw(Box<dyn Write + Send>, bool),
}



/**
 * Destination that can be written to.
 */
pub enum WritableDest<'a> {
    /// Writes directly to terminal.
    Terminal(&'a mut StandardStream),
    /// Writes to buffer.
    Buffered(&'a mut BufferWriter, Buffer),
    /// Writes to raw object.
    Raw(&'a mut (dyn Write + Send)),
    /// Writes to raw object with ANSI colors.
    ColoredRaw(Ansi<&'a mut (dyn Write + Send)>),
}


impl ColorConfig {
    /**
     * Convert color config to ansiterm color choise.
     */
    fn to_color_choice(self) -> ColorChoice {
        match self {
            ColorConfig::Always => ColorChoice::AlwaysAnsi,
            ColorConfig::Never => ColorChoice::Never,
            ColorConfig::Auto  => ColorChoice::Auto,
        }
    }
}


impl Destination {
    /**
     * Create destination from stdout stream.
     */
    pub fn from_stdout(config: ColorConfig) -> Self {
        let color = config.to_color_choice();
        if cfg!(windows) {
            // Global synchonization, no need to buffer.
            Destination::Terminal(StandardStream::stdout(color))
        } else {
            // Reiles on atomicity of `write` operation.
            Destination::Buffered(BufferWriter::stdout(color))
        }
    }

    
    /**
     * Create destination from stderr stream.
     */
    pub fn from_stderr(config: ColorConfig) -> Self {
        let color = config.to_color_choice();
        if cfg!(windows) {
            // Global synchonization, no need to buffer.
            Destination::Terminal(StandardStream::stderr(color))
        } else {
            // Reiles on atomicity of `write` operation.
            Destination::Buffered(BufferWriter::stderr(color))
        }
    }


    /**
     * Creates a writable destination.
     */
    pub fn writable(&mut self) -> WritableDest<'_> {
        match *self {
            Destination::Terminal(ref mut t) => WritableDest::Terminal(t),
            Destination::Buffered(ref mut t) => {
                let buf = t.buffer();
                WritableDest::Buffered(t, buf)
            }
            Destination::Raw(ref mut t, false) => WritableDest::Raw(t),
            Destination::Raw(ref mut t, true) => WritableDest::ColoredRaw(Ansi::new(t)),
        }
    }
}


impl<'a> WritableDest<'a> {
    /**
     * Returns the color spec based on the diagnostic level and style type.
     */
    pub fn apply_style(&mut self, level: &Level, style: &Style) -> io::Result<()> {
        let mut spec = ColorSpec::new();
        match style {
            Style::LineNumber => {
                spec.set_bold(true);
                spec.set_fg(Some(Color::Blue));
            }
            Style::UnderlinePrimary |
            Style::LabelPrimary => {
                if let Level::Fatal = level {
                    spec.set_fg(Some(Color::Red));
                    spec.set_bold(true);
                } else {
                    spec = level.color();
                }
            }
            Style::UnderlineSecondary |
            Style::LabelSecondary => {
                spec.set_bold(true);
                spec.set_fg(Some(Color::Blue));
            }
            Style::Level(lvl) => {
                spec = lvl.color();
            },
            Style::MainHeaderMsg => {
                spec.set_bold(true);
            }
            Style::HeaderMessage |
            Style::LineAndColumn |
            Style::Quotation |
            Style::NoStyle => {}
        };
        self.set_color(&spec)
    }


    /**
     * Set the current color of writable destination.
     */
    pub fn set_color(&mut self, color: &ColorSpec) -> io::Result<()> {
        match *self {
            WritableDest::Terminal(ref mut t) => t.set_color(color),
            WritableDest::Buffered(_, ref mut t) => t.set_color(color),
            WritableDest::ColoredRaw(ref mut t) => t.set_color(color),
            WritableDest::Raw(_) => Ok(()), 
        }
    }


    /**
     * Reset the current color settings to their original settings.
     * If there is a problem with resettin the color an error is returned.
     */
    pub fn reset(&mut self) -> io::Result<()> {
        match *self {
            WritableDest::Terminal(ref mut t) => t.reset(),
            WritableDest::Buffered(_, ref mut t) => t.reset(),
            WritableDest::ColoredRaw(ref mut t) => t.reset(),
            WritableDest::Raw(_) => Ok(()), 
        }
    }
}


impl<'a> Write for WritableDest<'a> {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        match *self {
            WritableDest::Terminal(ref mut t) => t.write(bytes),
            WritableDest::Buffered(_, ref mut buf) => buf.write(bytes),
            WritableDest::Raw(ref mut w) => w.write(bytes),
            WritableDest::ColoredRaw(ref mut t) => t.write(bytes),
        }
    }

    
    fn flush(&mut self) -> io::Result<()> {
        match *self {
            WritableDest::Terminal(ref mut t) => t.flush(),
            WritableDest::Buffered(_, ref mut buf) => buf.flush(),
            WritableDest::Raw(ref mut w) => w.flush(),
            WritableDest::ColoredRaw(ref mut w) => w.flush(),
        }
    }
}


impl<'a> Drop for WritableDest<'a> {
    fn drop(&mut self) {
        match *self {
            WritableDest::Buffered(ref mut dst, ref mut buf) => {
                drop(dst.print(buf));
            }
            _ => {}
        }
    }
}
