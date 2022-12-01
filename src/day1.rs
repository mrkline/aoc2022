use aoc_runner_derive::{aoc, aoc_generator};

#[aoc_generator(day1)]
fn clumper(input: &str) -> Vec<i64> {
    let mut parsed = Vec::new();
    let mut acc: i64 = 0;

    for line in input.lines() {
        if !line.is_empty() {
            acc += line.parse::<i64>().expect("not an int");
        } else {
            parsed.push(acc);
            acc = 0;
        }
    }
    parsed.push(acc);

    parsed
}

#[aoc(day1, part1)]
fn part1(input: &[i64]) -> i64 {
    *input.iter().max().expect("empty input")
}

#[aoc(day1, part2)]
fn part2(input: &[i64]) -> i64 {
    let mut maxes = [0i64; 3];

    for c in input {
        if *c > maxes[0] {
            maxes[0] = *c;
            // Blub blub
            if maxes[0] > maxes[1] {
                maxes.swap(0, 1);
            }
            if maxes[1] > maxes[2] {
                maxes.swap(1, 2);
            }
        }
    }
    maxes.iter().sum()
}
