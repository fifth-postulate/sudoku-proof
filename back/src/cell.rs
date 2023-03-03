use std::ops;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Cell(pub usize);

impl ops::Index<Cell> for Vec<Cell> {
    type Output = Cell;

    fn index(&self, index: Cell) -> &Self::Output {
        &self[index.0]
    }
}

impl ops::IndexMut<Cell> for Vec<Cell> {
    fn index_mut(&mut self, index: Cell) -> &mut Self::Output {
        &mut self[index.0]
    }
}

impl ops::Index<Cell> for Vec<usize> {
    type Output = usize;

    fn index(&self, index: Cell) -> &Self::Output {
        &self[index.0]
    }
}

impl ops::IndexMut<Cell> for Vec<usize> {
    fn index_mut(&mut self, index: Cell) -> &mut Self::Output {
        &mut self[index.0]
    }
}
