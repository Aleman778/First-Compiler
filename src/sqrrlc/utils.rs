
/***************************************************************************
 * Some common compiler utilities.
 ***************************************************************************/


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


impl ColorConfig {
    /**
     * Convert color config to ansiterm color choise.
     */
    fn to_color_choice(self) -> ColorChoice {
        match self {
            ColorConfig::Always => {
                if atty::is(atty::Stream::Stderr) {
                    ColorChoice::Always
                } else {
                    ColorChoice::AlwaysAnsi
                }
            }
            ColorConfig::Never => ColorChoice::Never,
            ColorConfig::Auto if atty::is(atty::Stream::Stderr) => ColorChoice::Auto,
            ColorConfig::Auto => ColorChoice::Never,
        }
    }

    
    /**
     * Returns true if this config should enable colors, false otherwise.
     */
    fn suggests_using_colors(self) -> bool {
        match self {
            ColorConfig::Always | ColorConfig::Auto => true,
            ColorConfig::Never => false,
        }
    }
}


/**
 * Destination enum defines different ways of writing
 * text both colored and non colored options.
 */
pub enum Destination {
    /// Writes directly to terminal using stdout/stderr
    Terminal(StandardStream),
    /// Writes first to buffer that can then write to terminal.
    Buffered(BufferWriter),
    /// Write to raw object with write and send traits, set bool true for colors.
    Raw(Box<dyn Write + Send>, bool),
}


impl Destination {
    fn from_stdout(color: ColorConfig) {
        
    }
        

    
    fn from_stderr(color: ColorChoice) {
        let mut dest = StandardStream::stderr(ColorChoice::AlwaysAnsi);
        
    }
}



enum WritableDst<'a> {
    Terminal(&mut StandardStream),
    Buffered(&mut BufferWriter),
    Raw(),
    ColoredRaw(),
}


impl<'a> WritableDest<'a> {

    
    /**
     * Set the current color of writable destination.
     */
    fn set_color(&mut self, color: &ColorSpec) -> io::Result<()> {
        match self {
            WritableDest::Terminal(&mut t) => t.set_color(color),
            WritableDest::Buffered(&mut t) => t.set_color(color),
            WritableDest::ColoredRaw(&mut t) => t.set_color(color),
            WritableDest::Raw(&mut _) => Ok(()), 
        }
    }


    /**
     * 
     */
    fn reset(&mut self, color: &ColorSpec) -> io::Result<()> {
        
    }

    pub fn write() {
        
    }
}
