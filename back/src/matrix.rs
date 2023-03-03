use std::fmt;

use crate::{cell::Cell, linkedlist::LinkedList};

pub struct Matrix {
    pub x: LinkedList,
    pub y: LinkedList,
    pub c: Vec<Cell>,
    pub size: Vec<usize>,
}

pub const H: Cell = Cell(0);

impl Matrix {
    pub fn new(n_columns: usize) -> Self {
        let mut result = Matrix {
            x: LinkedList::with_capacity(n_columns + 1),
            y: LinkedList::with_capacity(n_columns + 1),
            c: Vec::with_capacity(n_columns + 1),
            size: Vec::with_capacity(n_columns + 1),
        };
        assert_eq!(result.alloc_column(), H);
        for _ in 0..n_columns {
            result.add_column();
        }
        result
    }

    fn add_column(&mut self) {
        let column = self.alloc_column();
        self.x.insert(self.x[H].prev, column)
    }

    fn alloc_column(&mut self) -> Cell {
        let cell = self.alloc(H);
        self.c[cell] = cell;
        self.size.push(0);
        cell
    }

    fn alloc(&mut self, c: Cell) -> Cell {
        self.c.push(c);
        let cell = self.x.alloc();
        assert_eq!(self.y.alloc(), cell);
        cell
    }

    pub fn add_row(&mut self, row: &[bool]) {
        assert_eq!(row.len(), self.size.len() - 1);
        let mut c = H;
        let mut prev = None;
        for &is_filled in row {
            c = self.x[c].next;
            if is_filled {
                self.size[c] += 1;
                let cell = self.alloc(c);
                self.y.insert(self.y[c].prev, cell);
                if let Some(prev) = prev {
                    self.x.insert(prev, cell);
                }
                prev = Some(cell)
            }
        }
    }

    pub fn cover(&mut self, c: Cell) {
        self.x.remove(c);
        let mut i = self.y.cursor(c);
        while let Some(i) = i.next(&self.y) {
            let mut j = self.x.cursor(i);
            while let Some(j) = j.next(&self.x) {
                self.y.remove(j);
                self.size[self.c[j]] -= 1;
            }
        }
    }

    pub fn uncover(&mut self, c: Cell) {
        let mut i = self.y.cursor(c);
        while let Some(i) = i.prev(&self.y) {
            let mut j = self.x.cursor(i);
            while let Some(j) = j.prev(&self.x) {
                self.size[self.c[j]] += 1;
                self.y.restore(j);
            }
        }
        self.x.restore(c);
    }
}

impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "s: ")?;
        for s in &self.size {
            write!(f, "{:^5}", s)?
        }
        writeln!(f)?;

        write!(f, "c: ")?;
        for &Cell(c) in &self.c {
            write!(f, "{:^5}", c.saturating_sub(1))?;
        }
        writeln!(f)?;

        write!(f, "x: ")?;
        for link in &self.x.data {
            write!(f, " {:>1}|{:<1} ", link.prev.0, link.next.0)?
        }
        writeln!(f)?;

        write!(f, "y: ")?;
        for link in &self.y.data {
            write!(f, " {:>1}|{:<1} ", link.prev.0, link.next.0)?
        }
        writeln!(f)?;

        write!(f, "i: ")?;
        for i in 0..self.x.data.len() {
            write!(f, "{:^5}", i)?;
        }
        writeln!(f)?;

        Ok(())
    }
}
