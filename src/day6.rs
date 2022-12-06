use aoc_runner_derive::aoc;

use rustc_hash::FxHashSet;

fn unique_window_index(input: &str, w: usize) -> usize {
    let (idx, _) = input.as_bytes().windows(w).enumerate().find(|(_, w)| {
        let uniq: FxHashSet<_> = w.iter().copied().collect();
        uniq.len() == w.len()
    }).expect("no unique window");
    idx + w
}

#[aoc(day6, part1)]
fn part1(input: &str) -> usize {
    unique_window_index(input, 4)
}

#[aoc(day6, part2)]
fn part2(input: &str) -> usize {
    unique_window_index(input, 14)
}
