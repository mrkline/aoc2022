use aoc_runner_derive::aoc;

use std::str::FromStr;

use anyhow::anyhow;
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug)]
struct Step {
    count: usize,
    from: usize,
    to: usize,
}

impl FromStr for Step {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r#"move (\d+) from (\d+) to (\d+)"#).unwrap();
        }

        let caps = RE.captures(s).ok_or_else(|| anyhow!("bad move"))?;
        let count = caps[1].parse().unwrap();
        let from = caps[2].parse::<usize>().unwrap() - 1;
        let to = caps[3].parse::<usize>().unwrap() - 1;

        Ok(Self { count, from, to })
    }
}

#[derive(Debug)]
struct Input {
    stacks: Vec<Vec<u8>>,
    directions: Vec<Step>,
}

fn parse_stacks(input: &str) -> Vec<Vec<u8>> {
    let mut lines = input.lines().rev();

    let count = lines.next().unwrap().split_ascii_whitespace().count();
    let mut stacks = vec![vec![]; count];

    for l in lines.map(|l| l.as_bytes()) {
        for i in 0..count {
            let c = l[i * 4 + 1];
            if c != b' ' {
                stacks[i].push(c);
            }
        }
    }
    stacks
}

impl FromStr for Input {
    type Err = anyhow::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let halves: Vec<&str> = input.split("\n\n").collect();

        let stacks = parse_stacks(halves[0]);
        let directions = halves[1].lines().map(|l| l.parse().unwrap()).collect();

        Ok(Self { stacks, directions })
    }
}

fn as_char(ascii: u8) -> char {
    char::from_u32(ascii as u32).unwrap()
}

#[aoc(day5, part1)]
fn part1(i: &str) -> String {
    let mut input: Input = i.parse().unwrap();

    for step in input.directions {
        for _ in 0..step.count {
            let craned = input.stacks[step.from].pop().unwrap();
            input.stacks[step.to].push(craned);
        }
    }

    input
        .stacks
        .iter()
        .map(|s| as_char(*s.last().unwrap()))
        .collect()
}

#[aoc(day5, part2)]
fn part2(i: &str) -> String {
    let mut input: Input = i.parse().unwrap();

    for step in input.directions {
        let split_idx = input.stacks[step.from].len() - step.count;
        let craned = input.stacks[step.from].split_off(split_idx);
        input.stacks[step.to].extend(craned);
    }

    input
        .stacks
        .iter()
        .map(|s| as_char(*s.last().unwrap()))
        .collect()
}
