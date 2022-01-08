use std::collections::HashSet;

use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let commands = utils::read_first_line("./src/year2015/resources/day3.txt");

    println!("Part 1: {}", count_visited_houses(&commands));
    println!("Part 2: {}", count_visited_houses_with_aid(&commands));
}

fn count_visited_houses(commands: &String) -> usize {
    visit(commands).len()
}

fn count_visited_houses_with_aid(commands: &String) -> usize {
    let even = filter_commands(commands, |idx| idx % 2 == 0);
    let odd = filter_commands(commands, |idx| idx % 2 == 1);
    visit(&even).union(&visit(&odd)).collect::<HashSet<&(i32, i32)>>().len()
}

fn filter_commands<F: Fn(usize) -> bool>(commands: &String, predicate: F) -> String {
    commands.chars().enumerate()
        .filter(|&(i, _)| predicate(i))
        .map(|(_, cmd)| cmd)
        .collect::<String>()
}

fn visit(commands: &String) -> HashSet<(i32, i32)> {
    let mut coordinates = commands
        .chars()
        .scan((0, 0), |(x, y), cmd| {
            match cmd {
                '<' => *y -= 1,
                '>' => *y += 1,
                '^' => *x -= 1,
                _ => *x += 1
            }
            Some((*x, *y))
        }).collect_vec();
    coordinates.push((0, 0));

    HashSet::from_iter(coordinates)
}
