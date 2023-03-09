use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Source {
    pub file_path: Rc<String>,
    pub content: Rc<String>,
}

impl Source {
    pub fn new(file_path: String, content: String) -> Self {
        Self {
            file_path: Rc::new(file_path),
            content: Rc::new(content),
        }
    }
}