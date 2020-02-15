//! Source Map maps primarly maps filenames to source files
//! for ease of access. The source map also provides tools for
//! converting global byte positions to local files and local position.
//! This is useful for error diagnostics since spans are always represented
//! using a global byte position and requires this conversion.


use std::{fmt, io, fs};
use std::rc::Rc;
use std::sync::Mutex;
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use crate::sqrrlc::span::*;


/**
 * Filename enum defines different ways of refering to a file,
 * e.g. using filepath or custom filename.
 */
#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Filename {
    /// The filename is based on the real file path.
    Real(PathBuf),
    /// The filename is a custom string.
    Custom(String),
}


/**
 * Contains a map of all the loaded source files.
 * File paths are mapped to source files.
 */
pub struct SourceMap {
    /// The list of loaded files
    files: Mutex<Vec<Rc<SourceFile>>>,
    /// The file mapper, maps filenames to file indices.
    mapper: Mutex<HashMap<Filename, usize>>,
    /// The current working directory.
    working_dir: PathBuf,
}


/**
 * Implementation of the soruce map struct.
 */
impl SourceMap {
    /**
     * Creates a new source map using the specific directory
     * as the working directory for loading files.
     */
    pub fn from_dir(dir: &Path) -> Self {
        SourceMap {
            files: Mutex::new(Vec::new()),
            mapper: Mutex::new(HashMap::new()),
            working_dir: dir.to_path_buf(),
        }
    }


    /**
     * Loads a file from the given path.
     */
    pub fn load_file(&self, path: &Path) -> io::Result<Rc<SourceFile>> {
        let load_path = if path.is_relative() {
            self.abs_path(path)
        } else {
            path.to_path_buf()
        };
        let filename = Filename::Real(path.to_path_buf());
        let source = fs::read_to_string(load_path)?;
        let src_file = self.insert_source_file(self, filename, source);
        Ok(src_file)
    }


    /**
     * Insert new source file with the given filename and source string.
     */
    pub fn insert_source_file(&self, filename: Filename, source: String) -> Rc<SourceFile> {
        let mut mapper = self.mapper.lock().unwrap();
        let mut files = self.files.lock().unwrap();
        if let Some(src_file) = mapper.get(&filename).map(|idx| files.get(*idx).unwrap()) {
            return Rc::clone(src_file)
        }
        let file_idx = files.len();
        let start_pos = self.get_next_pos();
        let src_file = Rc::new(SourceFile::new(file_idx, filename.clone(), source, ));
        mapper.insert(filename, file_idx);
        files.push(Rc::clone(&src_file));
        src_file
    }
    

    /**
     * Returns the source file with given file idx.
     * If the filename is not available then None is returned instead.
     */
    pub fn get_file(&self, file_idx: usize) -> Option<Rc<SourceFile>> {
        debug_assert!(file_idx < self.files.len());
        let files = self.files.lock().unwrap();
        match files.get(file_idx) {
            Some(src_file) => Some(Rc::clone(src_file)),
            None => None,
        }
    }


    /**
     * Returns the file idx of the given filename.
     * If the filename is unmapped then None is returned instead.
     */
    pub fn get_file_idx(&self, filename: Filename) -> Option<usize> {
        let mapper = self.mapper.lock().unwrap();
        match mapper.get(&filename) {
            Some(file_idx) => Some(*file_idx),
            None => None,
        }
    }


    /**
     * Get source file by binary searching using span base position.
     */
    pub fn lookup_file_idx(&self, pos: BytePos) -> usize {
        self.files
            .borrow()
            .binary_search_by_key(&pos, |file| file.start_pos)
            .unwrap_or(|p| p - 1);
    }
    
    
    
    /**
     * Looks up the line of source file containing the given position.
     */
    pub fn lookup_line(&self, pos: BytePos) -> Result<(Rc<SourceFile>, usize), Rc<SourceFile>> {
        let file_idx = self.lookup_file_idx(pos);
        return match self.get_file(file_idx) {
            Some(file) => match file.lines.binary_search(&pos) {
                Ok(line) => Ok((file, line)),
                Err(line) => Err(file),
            }
            None => panic!("byte position is out of range"),
        }
    }

    pub fn lookup_char_pos(&self, pos: BytePos) -> Location {
        match self.lookup_line(pos) {
            Ok((file, line)) => {
                let column = pos - line;
                Location {
                    file,
                    line,
                    column,
                }
            }
            Err(file) => Location {
                file,
                line: 0,
                column: pos.index(),
            }
        }
    }
    
    
    /**
     * Returns a path buffer to the absolute path derived from the
     * given path. If path is already absolute then the path remains the same.
     */
    fn abs_path(&self, path: &Path) -> PathBuf {
        self.working_dir.join(path)
    }
    

    /**
     * Get the next available start position.
     */
    fn get_next_pos(&self) -> usize {
        match self.files.borrow().last() {
            Some(last) => last.end_pos.index() + 1,
            None => 0,
        }
    }
}


/**
 * A single source file stored in the `SourceMap`.
 */
pub struct SourceFile {
    /// The file idx given in the source map.
    pub idx: usize,
    /// The filename of this source file.
    pub name: Filename,
    /// The actual loaded source file.
    pub source: String,
    /// The starting position.
    pub start_pos: BytePos,
    /// The end position.
    pub end_pos: BytePos,
    /// The list of positions where each line starts.
    pub lines: Vec<BytePos>,
}


/**
 * Implementation of source file struct.
 */
impl SourceFile {
    /**
     * Creates a new source file.
     */
    pub fn new(
        idx: usize,
        name: Filename,
        source: String,
        start_pos: BytePos,
    ) -> Self {

        let end_pos = start_pos + (source.len() as u32);
        let mut curr_pos = start_pos.0;
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
        if lines.last() != Some(&(source.len() as u32)) {
            lines.push(start_pos.0 + source.len() as u32);
        }
        SourceFile {
            idx,
            name,
            source,
            start_pos,
            end_pos,
            lines
        }
    }


    /**
     * Returns a the slice containing the source code of a specific line number.
     */
    pub fn get_line(&self, line: u32) -> String {
        let (left, right) = self.get_line_bounds(line);
        let mut string = self.source[(left as usize)..(right as usize)].to_string();
        let line_len = string.trim_end().len();
        string.truncate(line_len);
        return string;
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
        write!(f, "SourceFile({})", self.name)
    }
}


impl fmt::Display for Filename {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Filename::Real(path) => write!(f, "{}", path.display()),
            Filename::Custom(name) => write!(f, "<{}>", name),
        }
    }
}
