use std::rc::Rc;

use crate::{ast::Ast, token::Span};

#[derive(Debug)]
pub struct Source {
    pub file_path: Rc<String>,
    pub content: Rc<String>,
    pub root: Option<Box<Ast>>,
    // TODO: Introduce a type table(?) that the Analyser can use for lookup
}

impl Source {
    pub fn new(file_path: String, content: String) -> Self {
        Self {
            file_path: Rc::new(file_path),
            content: Rc::new(content),
            root: None,
        }
    }

    pub fn maybe_slice_from(&self, span: Option<Span>) -> Option<&str> {
        if let Some(span) = span {
            if span.start + (span.len as usize) < self.content.len() {
                Some(&self.content[span.start..(span.start + span.len as usize)])
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn slice_from(&self, span: Span) -> Option<&str> {
        println!("SPAN: {span:?}");
        if span.start + (span.len as usize) <= self.content.len() {
            Some(&self.content[span.start..(span.start + span.len as usize)])
        } else {
            None
        }
    }
}

impl From<&'static str> for Source {
    fn from(value: &'static str) -> Self {
        Self {
            file_path: Rc::new("source".into()),
            content: Rc::new(value.into()),
            root: None,
        }
    }
}
