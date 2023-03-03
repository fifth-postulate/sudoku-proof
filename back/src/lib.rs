mod cell;
mod linkedlist;
mod matrix;

pub use matrix::Matrix;
use matrix::H;

pub fn solve(mut m: Matrix) -> usize {
    let mut n_answers = 0;
    go(&mut m, &mut n_answers);
    n_answers
}

fn go(m: &mut Matrix, n_answers: &mut usize) {
    let c = {
        let mut i = m.x.cursor(H);
        let mut c = match i.next(&m.x) {
            Some(it) => it,
            None => {
                *n_answers += 1;
                return;
            }
        };
        while let Some(next_c) = i.next(&m.x) {
            if m.size[next_c] < m.size[c] {
                c = next_c;
            }
        }
        c
    };

    m.cover(c);
    let mut r = m.y.cursor(c);
    while let Some(r) = r.next(&m.y) {
        let mut j = m.x.cursor(r);
        while let Some(j) = j.next(&m.x) {
            m.cover(m.c[j]);
        }
        go(m, n_answers);
        let mut j = m.x.cursor(r);
        while let Some(j) = j.prev(&m.x) {
            m.uncover(m.c[j]);
        }
    }
    m.uncover(c);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sample_problem() {
        let f = false;
        let t = true;

        let mut m = Matrix::new(7);
        m.add_row(&[f, f, t, f, t, t, f]);
        m.add_row(&[t, f, f, t, f, f, t]);
        m.add_row(&[f, t, t, f, f, t, f]);
        m.add_row(&[t, f, f, t, f, f, f]);
        m.add_row(&[f, t, f, f, f, f, t]);
        m.add_row(&[f, f, f, t, t, f, t]);

        let solutions = solve(m);

        assert_eq!(solutions, 1);
    }

    #[test]
    fn exhaustive_test() {
        'matrix: for bits in 0..=0b1111_1111_1111_1111 {
            let mut rows = [0u32; 4];
            for (i, row) in rows.iter_mut().enumerate() {
                *row = (bits >> (i * 4)) & 0b1111;
                if *row == 0 {
                    continue 'matrix;
                }
            }

            let brute_force = {
                let mut n_solutions = 0;
                for mask in 0..=0b1111 {
                    let mut or = 0;
                    let mut n_ones = 0;
                    for (i, &row) in rows.iter().enumerate() {
                        if mask & (1 << i) != 0 {
                            or |= row;
                            n_ones += row.count_ones()
                        }
                    }
                    if or == 0b1111 && n_ones == 4 {
                        n_solutions += 1;
                    }
                }
                n_solutions
            };

            let dlx = {
                let mut m = Matrix::new(4);
                for row_bits in rows.iter() {
                    let mut row = [false; 4];
                    for i in 0..4 {
                        row[i] = row_bits & (1 << i) != 0;
                    }
                    m.add_row(&row);
                }
                solve(m)
            };
            assert_eq!(brute_force, dlx)
        }
    }
}
