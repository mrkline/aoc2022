use aoc_runner_derive::{aoc, aoc_generator};

#[aoc_generator(day1)]
fn clumper(input: &str) -> Vec<i32> {
    let mut parsed = Vec::new();
    let mut acc: i32 = 0;

    for line in input.lines() {
        if !line.is_empty() {
            let i = line.parse::<i32>().expect("not an int");
            acc = acc.checked_add(i).expect("overflow");
        } else {
            parsed.push(acc);
            acc = 0;
        }
    }
    parsed.push(acc);

    parsed
}

#[aoc(day1, part1)]
fn part1(input: &[i32]) -> i32 {
    *input.iter().max().expect("empty input")
}

#[aoc(day1, part2)]
fn part2(input: &[i32]) -> i32 {
    let mut maxes = [0i32; 3];

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
