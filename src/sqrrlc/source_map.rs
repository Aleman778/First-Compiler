
/***************************************************************************
 * The `source_map` module holds the source code of the program that
 * is being compiled, 
 ***************************************************************************/


use std::{fmt, io, fs, env};
use std::rc::Rc;
use std::sync::Mutex;
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use crate::sqrrlc_ast::span::Span;


/**
 * Contains a map of all the loaded source files.
 * File paths are mapped to source files.
 */
pub struct SourceMap {
    /// The list of loaded files
    files: Mutex<Vec<Rc<SourceFile>>>,
    /// The file mapper, maps filenames to file ids.
    mapper: Mutex<HashMap<PathBuf, usize>>,
    /// The file loader used by the source mapper.
    file_loader: Box<dyn FileLoader + Sync + Send>,
}


/**
 * Implementation of the soruce map struct.
 */
impl SourceMap {
    /**
     * Creates a new source map from the current working directory.
     */
    pub fn new() -> Self {
        SourceMap {
            files: Mutex::new(Vec::new()),
            mapper: Mutex::new(HashMap::new()),
            file_loader: Box::new(SimpleFileLoader{
                working_dir: None,
            }),
        }
    }
    
    
    /**
     * Creates a new source map using the specific directory
     * as the working directory for loading files.
     */
    pub fn from_dir(dir: &Path) -> Self {
        SourceMap {
            files: Mutex::new(Vec::new()),
            mapper: Mutex::new(HashMap::new()),
            file_loader: Box::new(SimpleFileLoader{working_dir: Some(dir.to_path_buf())}),
        }
    }


    /**
     * Creates a new source map with a custom file loader.
     */
    pub fn with_file_loader(file_loader: Box<dyn FileLoader + Sync + Send>) -> Self {
        SourceMap {
            files: Mutex::new(Vec::new()),
            mapper: Mutex::new(HashMap::new()),
            file_loader,
        }
    }


    /**
     * Loads a file from the given path.
     */
    pub fn load_file(&self, path: &Path) -> io::Result<Rc<SourceFile>> {
        let mut mapper = self.mapper.lock().unwrap();
        let mut files = self.files.lock().unwrap();
        if let Some(file_id) = mapper.get(path) {
            if let Some(src_file) = files.get(*file_id) {
                 return Ok(Rc::clone(src_file))
            }
        }
        let source = self.file_loader.read_file(path)?;
        let filename = path.to_path_buf();
        let file_id = files.len();
        let src_file = Rc::new(SourceFile::new(filename.clone(), file_id, source, 0, 0));
        
        mapper.insert(filename, file_id);
        files.push(Rc::clone(&src_file));
        Ok(src_file)
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
    pub fn get_file_id(&self, filename: &Path) -> Option<usize> {
        let mapper = self.mapper.lock().unwrap();
        match mapper.get(&filename.to_path_buf()) {
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
        } + span.start.line;
    }
    

    /**
     * Check if a given file path exists.
     */
    pub fn file_exists(&self, path: &Path) -> bool {
        self.file_loader.file_exists(path)
    }
    
    
    /**
     * Returns a path buffer to the absolute path derived from the
     * given path. If path is already absolute then the path remains the same.
     */
    fn abs_path(&self, path: &Path) -> Option<PathBuf> {
        self.file_loader.abs_path(path)
    }
}


/**
 * File loader provides simple interface for loading source files.
 */
pub trait FileLoader {
    /**
     * Checks if the given path exists.
     */
    fn file_exists(&self, path: &Path) -> bool;

    
    /**
     * Returns the absoluate path of the given path.
     */
    fn abs_path(&self, path: &Path) -> Option<PathBuf>;


    /**
     * Tries to read a file at the given file path.
     */
    fn read_file(&self, path: &Path) -> io::Result<String>;
}


/**
 * Simple file loader that loads files with absolute
 * filenames based on either a given working_dir or
 * the actual working directory of running the compiler.
 * This is implemented using std::io library.
 */
pub struct SimpleFileLoader {
    working_dir: Option<PathBuf>,
}


impl FileLoader for SimpleFileLoader {
    /**
     * Simply checks that the file exists.
     */
    fn file_exists(&self, path: &Path) -> bool {
        path.exists()
    }


    /**
     * For relative paths the output will be the
     * working directory joined with the provided path.
     */
    fn abs_path(&self, path: &Path) -> Option<PathBuf> {
        if path.is_absolute() {
            Some(path.to_path_buf())
        } else {
            if let Some(dir) = &self.working_dir {
                Some(dir.join(path))
            } else {
                env::current_dir().ok().map(|cwd| cwd.join(path))
            }
        }
    }


    /**
     * Reads the given file path to string.
     */
    fn read_file(&self, path: &Path) -> io::Result<String> {
        fs::read_to_string(path)
    }
}


/**
 * A single source file stored in the `SourceMap`.
 */
pub struct SourceFile {
    /// The filename of this source file.
    pub filename: PathBuf,

    /// The file id given in the source map.
    pub file_id: usize,

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
        file_id: usize,
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
        SourceFile {
            filename,
            file_id,
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
        write!(f, "SourceFile({})", self.filename.display())
    }
}
