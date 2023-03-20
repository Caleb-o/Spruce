use super::token::Span;

pub struct Symbols {
    table: Vec<Span>,
}

impl Symbols {
    pub fn new() -> Self {
        Self { table: Vec::new() }
    }

    pub fn find_or_add(&mut self, span: &Span) -> u16 {
        for (idx, item) in self.table.iter().enumerate() {
            if span.slice_source() == item.slice_source() {
                return idx as u16;
            }
        }

        self.table.push(span.clone());
        self.table.len() as u16 - 1
    }
}