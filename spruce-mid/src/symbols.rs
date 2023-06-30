use spruce_front::{source::Source, token::Span};

#[derive(Debug, Clone)]
pub struct Symbols<'a> {
    source: &'a Source,
    table: Vec<Span>,
}

impl<'a> Symbols<'a> {
    pub fn new(source: &'a Source) -> Self {
        Self {
            source,
            table: Vec::new(),
        }
    }

    pub fn get_table(&self) -> &Vec<Span> {
        &self.table
    }

    pub fn find_or_add(&mut self, span: &Span) -> u32 {
        for (idx, item) in self.table.iter().enumerate() {
            if span.compare(self.source, item) {
                return idx as u32;
            }
        }

        self.table.push(span.clone());
        self.table.len() as u32 - 1
    }
}
