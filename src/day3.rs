use aoc_runner_derive::aoc;

use rustc_hash::FxHashSet;

fn calc_prio(item: u8) -> i64 {
    if (b'a'..=b'z').contains(&item) {
        (item - b'a') as i64 + 1
    } else if (b'A'..=b'Z').contains(&item) {
        (item - b'A') as i64 + 27
    } else {
        unreachable!("bad input");
    }
}

#[aoc(day3, part1)]
fn part1(input: &str) -> i64 {
    let mut prios = 0i64;

    for line in input.lines().map(|l| l.as_bytes()) {
        let half = line.len() / 2;
        let sack1 = to_set(&line[..half]);
        let sack2 = to_set(&line[half..]);

        let in_both = sack1.intersection(&sack2).collect::<Vec<_>>();
        assert_eq!(in_both.len(), 1);

        let in_both = *in_both[0];

        prios += calc_prio(in_both);
    }
    prios
}

fn to_set(l: &[u8]) -> FxHashSet<u8> {
    l.iter().copied().collect()
}

#[aoc(day3, part2)]
fn part2(input: &str) -> i64 {
    let mut prios = 0i64;
    for group in input
        .lines()
        .map(|l| to_set(l.as_bytes()))
        .collect::<Vec<_>>()
        .chunks_exact_mut(3)
    {
        let mut group = group.iter_mut();

        // Grab the first set.
        let mut set = FxHashSet::default();
        std::mem::swap(&mut set, group.next().unwrap());

        // Intersect it with the other two.
        for g in group {
            set = set.intersection(g).cloned().collect();
        }

        assert_eq!(set.len(), 1);
        prios += calc_prio(*set.iter().next().unwrap());
    }
    prios
}
