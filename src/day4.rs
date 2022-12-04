use aoc_runner_derive::{aoc, aoc_generator};

use std::ops::RangeInclusive;

type RangePair = (RangeInclusive<u8>, RangeInclusive<u8>);

#[aoc_generator(day4)]
fn pairs(input: &str) -> Vec<RangePair> {
    input
        .lines()
        .map(|l| {
            let ranges = l
                .split(',')
                .map(|r| {
                    let nums = r.split('-').collect::<Vec<_>>();
                    RangeInclusive::new(nums[0].parse().unwrap(), nums[1].parse().unwrap())
                })
                .collect::<Vec<_>>();
            (ranges[0].clone(), ranges[1].clone())
        })
        .collect()
}

#[aoc(day4, part1)]
fn part1(input: &[RangePair]) -> usize {
    input
        .iter()
        .filter(|pairs| {
            let (smaller, bigger) = if pairs.0.len() <= pairs.1.len() {
                (&pairs.0, &pairs.1)
            } else {
                (&pairs.1, &pairs.0)
            };
            bigger.start() <= smaller.start() && bigger.end() >= smaller.end()
        })
        // .map(|m| {println!("Overlap: {:?}", m); m})
        .count()
}

#[aoc(day4, part2)]
fn part2(input: &[RangePair]) -> usize {
    input
        .iter()
        .filter(|pairs| pairs.0.start() <= pairs.1.end() && pairs.0.end() >= pairs.1.start())
        .count()
}
