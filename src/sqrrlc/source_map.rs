
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
 * Contains a map of all the loaded source files.
 * File paths are mapped to source files.
 */
#[derive(Debug)]
pub struct SourceMap {
    /// The list of loaded files
    files: Mutex<Vec<Rc<SourceFile>>>,

    /// The file mapper, maps filenames to file ids.
    mapper: Mutex<HashMap<PathBuf, usize>>,
    
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
    pub fn new(cwd: PathBuf) -> Self {
        SourceMap {
            files: Mutex::new(Vec::new()),
            mapper: Mutex::new(HashMap::new()),
            working_dir: cwd,
        }
    }


    /**
     * Loads a file from the given path.
     */
    pub fn load_file(&self, path: &Path) -> io::Result<Rc<SourceFile>> {
        let mut mapper = self.mapper.lock().unwrap();
        let mut files = self.files.lock().unwrap();

        // Check if the file already is loaded,
        // then simply return that instead of reloading!
        // if mapper.contains_key(path) {
            // let file_id = mapper.get(path).unwrap();
            // return Ok(files[file_id]); what happens here? file_id should be ok?
        // }
        
        let source = fs::read_to_string(self.abs_path(&path))?;
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
