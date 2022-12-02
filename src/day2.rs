use aoc_runner_derive::aoc;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Move {
    Rock,
    Paper,
    Scissors,
}

impl From<u8> for Move {
    fn from(b: u8) -> Self {
        match b {
            0 => Move::Rock,
            1 => Move::Paper,
            2 => Move::Scissors,
            _ => panic!("bad input"),
        }
    }
}

fn value_of(m: Move) -> i64 {
    match m {
        Move::Rock => 1,
        Move::Paper => 2,
        Move::Scissors => 3,
    }
}

fn find_loser(m: Move) -> Move {
    match m {
        Move::Rock => Move::Scissors,
        Move::Paper => Move::Rock,
        Move::Scissors => Move::Paper,
    }
}

fn find_winner(m: Move) -> Move {
    match m {
        Move::Rock => Move::Paper,
        Move::Paper => Move::Scissors,
        Move::Scissors => Move::Rock,
    }
}

#[aoc(day2, part1)]
fn part1(input: &str) -> i64 {
    let mut score = 0i64;

    for line in input.lines() {
        let line = line.as_bytes();
        let (them, us): (Move, Move) = ((line[0] - b'A').into(), (line[2] - b'X').into());

        score += value_of(us);

        score += match (them, us) {
            (l, r) if l == r => 3,
            (l, r) if r == find_winner(l) => 6,
            (_, _) => 0,
        }
    }
    score
}

#[aoc(day2, part2)]
fn part2(input: &str) -> i64 {
    let mut score = 0i64;

    for line in input.lines() {
        let line = line.as_bytes();
        let them: Move = (line[0] - b'A').into();

        match line[2] {
            b'X' => score += value_of(find_loser(them)),
            b'Y' => score += value_of(them) + 3,
            b'Z' => score += value_of(find_winner(them)) + 6,
            _ => panic!("bad input"),
        }
    }
    score
}
