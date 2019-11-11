
/***************************************************************************
 * The `source_map` module holds the source code of the program that
 * is being compiled, 
 ***************************************************************************/


use std::{fmt, io, fs};
use std::rc::Rc;
use std::sync::Mutex;
use std::path::{Path, PathBuf};
use std::collections::HashMap;


/**
 * Contains a map of all the loaded source files.
 * File paths are mapped to source files.
 */
#[derive(Debug)]
pub struct SourceMap {
    /// The mapping of file paths to sourec files.
    pub files: Mutex<HashMap<PathBuf, Rc<SourceFile>>>,

    /// The current working directory.
    pub working_dir: PathBuf,
}


/**
 * Implementation of the soruce map struct.
 */
impl SourceMap {
    /**
     * Creates a new source map
     */
    pub fn new(dir: PathBuf) -> Self {
        SourceMap {
            files: Mutex::new(HashMap::new()),
            working_dir: dir,
        }
    }


    /**
     * Loads a file from the given path.
     */
    pub fn load_file(&self, path: &Path) -> io::Result<Rc<SourceFile>> {
        let source = fs::read_to_string(self.abs_path(&path))?;
        let filename = path.to_path_buf();
        let src_file = Rc::new(SourceFile::new(filename.clone(), source, 0, 0));
        let mut files = self.files.lock().unwrap();
        files.insert(filename, Rc::clone(&src_file));
        Ok(src_file)
    }


    /**
     * Returns the source file with the given filename,
     * if file is not loaded None is returned instead.
     */
    pub fn get_file(&self, filename: &Path) -> Option<Rc<SourceFile>> {
        let files = self.files.lock().unwrap();
        match files.get(&filename.to_path_buf()) {
            Some(src_file) => Some(Rc::clone(src_file)),
            None => None,
        }
    }


    /**
     * Check if a given file path exists.
     */
    pub fn file_exists(&self, path: &Path) -> bool {
        fs::metadata(path).is_ok()
    }
    
    
    /**
     * Returns a path buffer to the absolute path derived from the
     * given path. If path is already absolute then the path remains the same.
     */
    fn abs_path(&self, path: &Path) -> PathBuf {
        if path.is_absolute() {
            path.to_path_buf()
        } else {
            self.working_dir.join(path)
        }
    }
}



/**
 * A single source file stored in the `SourceMap`.
 */
pub struct SourceFile {
    /// The filename of this source file.
    pub filename: PathBuf,

    /// The actual loaded source file.
    pub source: String,

    /// The starting line.
    pub start_line: u32,

    /// The starting position.
    pub start_pos: u32,

    /// The end position.
    pub end_pos: u32,

    /// The list of positions where each line starts.
    pub lines: Vec<u32>,
}


/**
 * Implementation of source file struct.
 */
impl SourceFile {
    /**
     * Creates a new source file.
     */
    pub fn new(
        filename: PathBuf,
        source: String,
        start_line: u32,
        start_pos: u32
    ) -> Self {

        let end_pos = start_pos + (source.len() as u32);
        let mut curr_pos = 0;
        let mut lines = vec![start_pos];
        let mut remaining = source.as_str();
        loop {
            match remaining.find("\n") {
                Some(pos) => {
                    curr_pos += (pos as u32) + 1;
                    lines.push(curr_pos);
                    remaining = remaining.split_at(pos + 1).1;
                },
                None => break,
            }
        }
        SourceFile {
            filename,
            source,
            start_line,
            start_pos,
            end_pos,
            lines
        }
    }


    /**
     * Returns a the slice containing the source code of a specific line number.
     */
    pub fn get_line(&self, line: u32) -> &str {
        let (left, right) = self.get_line_bounds(line);
        &self.source[(left as usize)..(right as usize)]
    }


    /**
     * Returns the left and right position of the line in the source code.
     */
    pub fn get_line_bounds(&self, line: u32) -> (u32, u32) {
        let line = line as usize;
        (self.lines[line - 1], self.lines[line])
    }
}


/**
 * Debug formatting for source file.
 * Do not need to print entire source code.
 */
impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SourceFile({})", self.filename.display())
    }
}
