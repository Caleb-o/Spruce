use std::rc::Rc;

use crate::object::Object;

pub struct Heap {
    allocs: u32,
    next_sweep: u32,
    pub buffer: Vec<Option<Rc<Object>>>,
}

impl Heap {
    pub fn new() -> Self {
        Self { allocs: 0, next_sweep: 8, buffer: Vec::with_capacity(32) }
    }

    pub fn clear(&mut self) {
        self.buffer.clear();
        self.buffer.reserve(32);

        self.allocs = 0;
        self.next_sweep = 8;
    }

    pub fn find_next(&mut self, object: Object) -> Rc<Object> {
        self.allocs += 1;

        if self.allocs >= self.next_sweep {
            self.sweep();
        }

        let item = Rc::new(object);
        for i in 0..self.buffer.len() {
            if self.buffer[i].is_none() {
                self.buffer[i] = Some(Rc::clone(&item));
                return item;
            }
        }

        self.buffer.push(Some(Rc::clone(&item)));
        item
    }

    fn sweep(&mut self) {
        self.next_sweep *= 2;

        for idx in 0..self.buffer.len() {
            if let Some(item) = &self.buffer[idx] {
                if Rc::strong_count(item) == 1 {
                    self.buffer[idx] = None;
                }
            }
        }
    }
}