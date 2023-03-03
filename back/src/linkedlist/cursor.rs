use crate::{cell::Cell, linkedlist::LinkedList};

pub struct Cursor {
    pub head: Cell,
    pub current: Cell,
}

impl Cursor {
    pub fn next(&mut self, list: &LinkedList) -> Option<Cell> {
        self.current = list[self.current].next;
        if self.current != self.head {
            Some(self.current)
        } else {
            None
        }
    }

    pub fn prev(&mut self, list: &LinkedList) -> Option<Cell> {
        self.current = list[self.current].prev;
        if self.current != self.head {
            Some(self.current)
        } else {
            None
        }
    }
}
