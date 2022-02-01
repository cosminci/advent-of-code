use itertools::Itertools;
use lazy_static::lazy_static;
use regex::{Match, Regex};
use substring::Substring;

use crate::utils;

pub fn solve() {
    let data = utils::read_first_line("./src/year2016/resources/day9.txt");

    println!("Part 1: {}", decompressed_length_1(data.as_str()));
    println!("Part 2: {}", decompressed_length_2(data.as_str()));
}

lazy_static! {
    static ref MARKER_REGEX: Regex = Regex::new("\\(([0-9])+x([0-9]+)\\)").unwrap();
}

fn decompressed_length_1(data: &str) -> usize {
    match MARKER_REGEX.find(data) {
        None => data.len(),
        Some(matched) => {
            let (count, repeat_group, remaining) = parse_marker(data, matched);
            matched.start() + count * repeat_group.len() +
                decompressed_length_1(remaining)
        }
    }
}

fn decompressed_length_2(data: &str) -> usize {
    match MARKER_REGEX.find(data) {
        None => data.len(),
        Some(matched) => {
            let (count, repeat_group, remaining) = parse_marker(data, matched);
            matched.start() + count * decompressed_length_2(repeat_group) +
                decompressed_length_2(remaining)
        }
    }
}

fn parse_marker<'a>(data: &'a str, matched: Match) -> (usize, &'a str, &'a str) {
    let parts = data
        .substring(matched.start() + 1, matched.end() - 1)
        .split('x')
        .collect_vec();
    let length = parts[0].parse::<usize>().unwrap();
    let count = parts[1].parse::<usize>().unwrap();
    let tail = data.substring(matched.end(), data.len());
    let (repeat_group, remaining) = tail.split_at(length);
    (count, repeat_group, remaining)
}
