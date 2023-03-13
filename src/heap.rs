use std::rc::Rc;

use crate::object::Object;

const CAPACITY_INIT: usize = 32;
const NEXT_SWEEP_INIT: u32 = 8;

pub struct Heap {
    allocs: u32,
    next_sweep: u32,
    pub buffer: Vec<Option<Rc<Object>>>,
}

impl Heap {
    pub fn new() -> Self {
        Self { allocs: 0, next_sweep: NEXT_SWEEP_INIT, buffer: Vec::with_capacity(CAPACITY_INIT) }
    }

    pub fn clear(&mut self) {
        self.buffer.clear();
        self.buffer.reserve(CAPACITY_INIT);

        self.allocs = 0;
        self.next_sweep = NEXT_SWEEP_INIT;
    }

    pub fn find_next(&mut self, object: Object) -> Rc<Object> {
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
        
        self.allocs += 1;
        self.buffer.push(Some(Rc::clone(&item)));
        item
    }

    fn sweep(&mut self) {
        self.next_sweep = (self.next_sweep as f32 * 1.5) as u32;

        for idx in 0..self.buffer.len() {
            if let Some(item) = &self.buffer[idx] {
                if Rc::strong_count(item) == 1 {
                    self.buffer[idx] = None;
                }
            }
        }
    }
}