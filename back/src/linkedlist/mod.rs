pub mod cursor;
mod link;

use self::{cursor::Cursor, link::Link};
use crate::cell::Cell;
use std::ops;

#[derive(Default, Debug)]
pub struct LinkedList {
    pub data: Vec<Link>,
}

impl ops::Index<Cell> for LinkedList {
    type Output = Link;

    fn index(&self, index: Cell) -> &Self::Output {
        &self.data[index]
    }
}

impl ops::IndexMut<Cell> for LinkedList {
    fn index_mut(&mut self, index: Cell) -> &mut Self::Output {
        &mut self.data[index]
    }
}

impl LinkedList {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    pub fn alloc(&mut self) -> Cell {
        let cell = Cell(self.data.len());
        self.data.push(Link::new(cell));
        cell
    }

    pub fn insert(&mut self, a: Cell, b: Cell) {
        let c = self[a].next;

        self[b].prev = a;
        self[b].next = c;

        self[a].next = b;
        self[c].prev = b;
    }

    pub fn remove(&mut self, b: Cell) {
        let a = self[b].prev;
        let c: Cell = self[b].next;

        self[a].next = c;
        self[c].prev = a;
    }

    pub fn restore(&mut self, b: Cell) {
        let a = self[b].prev;
        let c: Cell = self[b].next;

        self[a].next = b;
        self[c].prev = b;
    }

    pub fn cursor(&self, head: Cell) -> Cursor {
        Cursor {
            head,
            current: head,
        }
    }
}
