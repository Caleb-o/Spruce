use std::rc::Rc;

use super::{token::Span, sprucetype::SpruceType};

#[derive(Debug, Clone)]
pub struct Local {
    pub identifier: Span,
    pub depth: u16,
    pub position: u16,
    pub mutable: bool,
    pub kind: Rc<SpruceType>,
}

const GLOBAL_DEPTH: u16 = 0;

pub struct SymTable {
    depth: u16,
    depth_limit: u16,
    marked: bool,
    locals: Vec<Local>,
}

impl SymTable {
    pub fn new() -> Self {
        Self { depth: 0, depth_limit: 0, marked: false, locals: Vec::new() }
    }

    #[inline]
    pub fn get_depth(&self) -> u16 {
        self.depth
    }

    #[inline]
    pub fn new_scope(&mut self) {
        self.depth += 1;
    }

    #[inline]
    pub fn is_global(&self) -> bool {
        self.depth == GLOBAL_DEPTH
    }

    #[inline]
    pub fn mark_depth_limit(&mut self) {
        if self.marked {
            return;
        }
        self.marked = true;
        self.depth_limit = self.depth;
    }

    #[inline]
    pub fn reset_mark(&mut self) {
        self.marked = false;
        self.depth_limit = 0;
    }

    #[inline]
    pub fn close_scope(&mut self) {
        assert!(self.depth > 0);
        let start = self.count_scope() as usize;
        self.depth -= 1;
        self.locals.drain(self.locals.len() - start..);
    }

    pub fn new_local(
        &mut self,
        identifier: Span,
        mutable: bool,
        kind: Rc<SpruceType>,
    ) {
        let position = self.find_count();
        self.locals.push(Local { 
            identifier,
            depth: self.depth,
            position,
            mutable,
            kind,
        });
    }

    pub fn find_local(&self, span: &Span, anyscope: bool) -> Option<&Local> {
        for local in self.locals.iter().rev() {
            if anyscope && (local.depth == GLOBAL_DEPTH || local.depth >= self.depth_limit) {
                if local.identifier.compare(span) {
                    return Some(local);
                }
            }
        }

        None
    }

    #[inline]
    fn find_count(&self) -> u16 {
        if self.depth > GLOBAL_DEPTH {
            self.find_local_count()
        } else {
            self.find_global_count()
        }
    }

    fn count_scope(&self) -> u16 {
        self.locals
            .iter()
            .rev()
            .fold(0, |acc, l| {
                if l.depth >= self.depth {
                    acc + 1
                } else {
                    acc
                }
            })
    }

    fn find_local_count(&self) -> u16 {
        self.locals
            .iter()
            .rev()
            .fold(0, |acc, l| {
                if l.depth >= self.depth_limit {
                    acc + 1
                } else {
                    acc
                }
            })
    }

    fn find_global_count(&self) -> u16 {
        self.locals
            .iter()
            .fold(0, |acc, l| {
                if l.depth == GLOBAL_DEPTH {
                    acc + 1
                } else {
                    acc
                }
            })
    }
}