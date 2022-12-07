use aoc_runner_derive::{aoc, aoc_generator};

use rustc_hash::FxHashMap;

type Directory = FxHashMap<String, Node>;

#[derive(Debug)]
enum Node {
    Dir(Directory),
    File(i64),
}

#[aoc_generator(day7)]
fn generate(input: &str) -> Directory {
    let mut lines = input.lines().peekable();
    assert_eq!(lines.next(), Some("$ cd /"));

    let mut root = Directory::default();
    generate_dir(&mut root, &mut lines);
    assert_eq!(lines.next(), None);
    root
}

fn generate_dir<'a>(dir: &mut Directory, lines: &mut std::iter::Peekable<std::str::Lines<'a>>) {
    while let Some(line) = lines.next() {
        assert!(line.starts_with("$ "));
        if line == "$ cd .." {
            return;
        } else if line == "$ ls" {
            while lines.peek().is_some() && !lines.peek().unwrap().starts_with("$ ") {
                let entry = lines.next().unwrap();
                if entry.starts_with("dir ") {
                    let (_, name) = entry.split_at(4);
                    dir.insert(name.to_owned(), Node::Dir(Directory::default()));
                } else {
                    let tokens: Vec<_> = entry.split_ascii_whitespace().collect();
                    assert_eq!(tokens.len(), 2);
                    dir.insert(tokens[1].to_owned(), Node::File(tokens[0].parse().unwrap()));
                }
            }
        } else {
            assert!(line.starts_with("$ cd "));
            let (_, to) = line.split_at(5);
            match &mut dir.get_mut(to) {
                Some(Node::Dir(d)) => generate_dir(d, lines),
                Some(Node::File(_)) => panic!("can't cd a file"),
                None => panic!("Unknown node {to}"),
            }
        }
    }
}

fn traverse_dirs<F: FnMut(&Directory)>(d: &Directory, f: &mut F) {
    for child in d.values() {
        if let Node::Dir(c) = child {
            f(c);
            traverse_dirs(c, f);
        }
    }
}

fn dir_size(d: &Directory) -> i64 {
    d.values()
        .map(|child| match child {
            Node::Dir(c) => dir_size(c),
            Node::File(s) => *s,
        })
        .sum()
}

#[aoc(day7, part1)]
fn part1(input: &Directory) -> i64 {
    let mut under_100k = 0;

    let mut fold = |d: &Directory| {
        let s = dir_size(d);
        if s <= 100000 {
            under_100k += s;
        }
    };

    traverse_dirs(input, &mut fold);

    under_100k
}

#[aoc(day7, part2)]
fn part2(input: &Directory) -> i64 {
    let available = 70000000 - dir_size(input);
    let needed = 30000000 - available;

    let mut axe_size = i64::MAX;

    let mut fold = |d: &Directory| {
        let s = dir_size(d);
        if s >= needed && s < axe_size {
            axe_size = s;
        }
    };

    traverse_dirs(input, &mut fold);

    axe_size
}
