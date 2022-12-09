use aoc_runner_derive::{aoc, aoc_generator};

use std::fmt::{Display, Formatter, Error};

#[derive(Debug, Clone)]
struct Grid<T> {
    cells: Vec<T>,
    stride: usize
}

impl<T: Display> Display for Grid<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for line in self.cells.chunks(self.stride) {
            for x in 0..self.stride {
                write!(f, "{}", line[x])?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<T: Default + Clone> Grid<T> {
    fn new(width: usize, height: usize) -> Self {
        Self { cells: vec![T::default(); width * height], stride: width }
    }

    fn at(&self, x: usize, y: usize) -> &T {
        &self.cells[x + y * self.stride]
    }

    fn at_mut(&mut self, x: usize, y: usize) -> &mut T {
        &mut self.cells[x + y * self.stride]
    }
}

#[aoc_generator(day8)]
fn generate(input: &str) -> Grid<i8> {
    let mut grid = None;

    for (y, line) in input.lines().enumerate() {
        if y == 0 {
            grid = Some(Grid::new(line.len(), line.len()));
        }

        let grid = grid.as_mut().unwrap();
        for x in 0..line.len() {
            *grid.at_mut(x, y) = (line.as_bytes()[x] - b'0') as i8;
        }
    }

    grid.unwrap()
}

fn seen_grid(input: &Grid<i8>) -> Grid<bool> {
    let mut seen = Grid::<bool>::new(input.stride, input.stride);

    // Look from each side
    for x in 0..seen.stride {
        let mut tallest = i8::MIN;
        for y in 0..seen.stride {
            let ci = *input.at(x, y);
            if ci > tallest {
                tallest = ci;
                *seen.at_mut(x, y) = true;
            }
        }
    }
    for x in 0..seen.stride {
        let mut tallest = i8::MIN;
        for y in (0..seen.stride).rev() {
            let ci = *input.at(x, y);
            if ci > tallest {
                tallest = ci;
                *seen.at_mut(x, y) = true;
            }
        }
    }
    for y in 0..seen.stride {
        let mut tallest = i8::MIN;
        for x in 0..seen.stride {
            let ci = *input.at(x, y);
            if ci > tallest {
                tallest = ci;
                *seen.at_mut(x, y) = true;
            }
        }
    }
    for y in 0..seen.stride {
        let mut tallest = i8::MIN;
        for x in (0..seen.stride).rev() {
            let ci = *input.at(x, y);
            if ci > tallest {
                tallest = ci;
                *seen.at_mut(x, y) = true;
            }
        }
    }

    seen
}

#[aoc(day8, part1)]
fn part1(input: &Grid<i8>) -> usize {
    seen_grid(input).cells.iter().filter(|c| **c).count()
}

fn view_from(input: &Grid<i8>, x: usize, y: usize) -> i8 {
    let mut score = 1;
    let left = 0;

}

#[aoc(day8, part2)]
fn part2(input: &Grid<i8>) -> i8 {
    let mut score = Grid::<i8>::new(input.stride, input.stride);

    // Look from each side
    for x in 0..score.stride {
        let mut tallest = i8::MIN;
        for y in 0..score.stride {
            let ci = *input.at(x, y);
            if ci > tallest {
                tallest = ci;
                *score.at_mut(x, y) = y as i8;
            }
        }
    }
    for x in 0..score.stride {
        let mut tallest = i8::MIN;
        for y in (0..score.stride).rev() {
            let ci = *input.at(x, y);
            if ci > tallest {
                tallest = ci;
                *score.at_mut(x, y) += (score.stride - y - 1) as i8;
            }
        }
    }
    for y in 0..score.stride {
        let mut tallest = i8::MIN;
        for x in 0..score.stride {
            let ci = *input.at(x, y);
            if ci > tallest {
                tallest = ci;
                *score.at_mut(x, y) += x as i8;
            }
        }
    }
    for y in 0..score.stride {
        let mut tallest = i8::MIN;
        for x in (0..score.stride).rev() {
            let ci = *input.at(x, y);
            if ci > tallest {
                tallest = ci;
                *score.at_mut(x, y) += (score.stride - x - 1) as i8;
            }
        }
    }

    println!("{}", score);
    *score.cells.iter().max().unwrap()
}
