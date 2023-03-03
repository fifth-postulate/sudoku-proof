use crate::cell::Cell;
use std::ops;

#[derive(Debug)]
pub struct Link {
    pub prev: Cell,
    pub next: Cell,
}

impl Link {
    pub fn new(cell: Cell) -> Self {
        Self {
            prev: cell,
            next: cell,
        }
    }
}

impl ops::Index<Cell> for Vec<Link> {
    type Output = Link;

    fn index(&self, index: Cell) -> &Self::Output {
        &self[index.0]
    }
}

impl ops::IndexMut<Cell> for Vec<Link> {
    fn index_mut(&mut self, index: Cell) -> &mut Self::Output {
        &mut self[index.0]
    }
}
