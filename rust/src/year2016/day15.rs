use num::Integer;
use regex::Regex;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day15.txt");
    let mut discs = parse_input(input);

    println!("Part 1: {}", earliest_time_to_escape(&discs));
    discs.push(Disc { idx: discs.len() + 1, size: 11, start: 0 });
    println!("Part 2: {}", earliest_time_to_escape(&discs));
}

struct Disc {
    idx: usize,
    size: usize,
    start: usize,
}

fn earliest_time_to_escape(discs: &Vec<Disc>) -> usize {
    discs.iter().fold((0, 1), synchronize_discs).0
}

fn synchronize_discs((t0, step): (usize, usize), disc: &Disc) -> (usize, usize) {
    (t0..).step_by(step).find_map(|t| {
        if (disc.start + disc.idx + t) % disc.size == 0 {
            Some((t, step.lcm(&disc.size)))
        } else {
            None
        }
    }).unwrap()
}

fn parse_input(input: Vec<String>) -> Vec<Disc> {
    let re = "Disc #([0-9]) has ([0-9]+) positions; at time=0, it is at position ([0-9]+).";
    let disc_regex = Regex::new(re).unwrap();
    input.iter()
        .flat_map(|line| disc_regex.captures(line))
        .map(|captures| Disc {
            idx: captures[1].parse().unwrap(),
            size: captures[2].parse().unwrap(),
            start: captures[3].parse().unwrap(),
        }).collect()
}
