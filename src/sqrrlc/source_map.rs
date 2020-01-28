
/***************************************************************************
 * The `source_map` module holds the source code of the program that
 * is being compiled, 
 ***************************************************************************/


use std::{fmt, io, fs};
use std::rc::Rc;
use std::sync::Mutex;
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use crate::sqrrlc_ast::span::Span;


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
    /// The file mapper, maps filenames to file ids.
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
        let mut mapper = self.mapper.lock().unwrap();
        let mut files = self.files.lock().unwrap();
        if let Some(file_id) = mapper.get(&Filename::Real(path.to_path_buf())) {
            if let Some(src_file) = files.get(*file_id) {
                return Ok(Rc::clone(src_file))
            }
        }
        let source = fs::read_to_string(path)?;
        let filename = Filename::Real(path.to_path_buf());
        let file_id = files.len();
        let src_file = Rc::new(SourceFile::new(file_id, filename.clone(), source, 0, 0));
        
        mapper.insert(filename, file_id);
        files.push(Rc::clone(&src_file));
        Ok(src_file)
    }


    /**
     * Adds source code without using any files.
     */
    pub fn add_from_source(
        &self,
        filename: Filename,
        source: String,
        line: u32,
        col: u32
    ) -> Rc<SourceFile> {
        let mut mapper = self.mapper.lock().unwrap();
        let mut files = self.files.lock().unwrap();
        if let Some(src_file) = mapper.get(&filename).map(|id| files.get(*id).unwrap()) {
            return Rc::clone(src_file)
        }
        let file_id = files.len();
        let src_file = Rc::new(SourceFile::new(file_id, filename.clone(), source, line, col));
        mapper.insert(filename, file_id);
        files.push(Rc::clone(&src_file));
        src_file
    }


    /**
     * Returns the source file with given file id.
     * If the filename is not available then None is returned instead.
     */
    pub fn get_file(&self, file_id: usize) -> Option<Rc<SourceFile>> {
        let files = self.files.lock().unwrap();
        match files.get(file_id) {
            Some(src_file) => Some(Rc::clone(src_file)),
            None => None,
        }
    }


    /**
     * Returns the file id of the given filename.
     * If the filename is unmapped then None is returned instead.
     */
    pub fn get_file_id(&self, filename: Filename) -> Option<usize> {
        let mapper = self.mapper.lock().unwrap();
        match mapper.get(&filename) {
            Some(file_id) => Some(*file_id),
            None => None,
        }
    }
    
    
    /**
     * Returns the line number from looked up file based on the provided span.
     * If the file is not loaded then assume it starts at line 0.
     */
    pub fn lookup_linum(&self, span: &Span) -> u32 {
        let file = self.get_file(span.loc);
        return match file {
            Some(file) => (&file).start_line,
            None => 0u32,
        }
    }
    
    
    /**
     * Returns a path buffer to the absolute path derived from the
     * given path. If path is already absolute then the path remains the same.
     */
    fn abs_path(&self, path: &Path) -> PathBuf {
        self.working_dir.join(path)
    }
}


/**
 * A single source file stored in the `SourceMap`.
 */
pub struct SourceFile {
    /// The file id given in the source map.
    pub id: usize,
    
    /// The filename of this source file.
    pub filename: Filename,

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
        id: usize,
        filename: Filename,
        source: String,
        start_line: u32,
        start_pos: u32,
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
        if lines.last() != Some(&(source.len() as u32)) {
            lines.push(source.len() as u32);
        }
        SourceFile {
            id,
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
        write!(f, "SourceFile({})", self.filename)
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
