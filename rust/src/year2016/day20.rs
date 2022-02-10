use std::collections::VecDeque;

use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day20.txt");
    let intervals = merge_intervals(parse_input(input));

    let blacklists = merge_intervals(intervals);
    println!("Part 1: {}", first_allowed_ip(&blacklists));
    println!("Part 2: {}", count_allowed_ips(&blacklists, 0, 4294967295));
}

fn first_allowed_ip(blacklists: &Vec<(usize, usize)>) -> usize {
    blacklists[0].1 + 1
}

fn count_allowed_ips(blacklists: &Vec<(usize, usize)>, min_ip: usize, max_ip: usize) -> usize {
    let (first_left, first_right) = blacklists[0];
    let (count, last_right) = blacklists
        .iter()
        .dropping(1)
        .fold((0, first_right), |(count, prev_right), (curr_left, curr_right)| {
            (count + curr_left - prev_right - 1, *curr_right)
        });
    (first_left - min_ip) + count + (max_ip - last_right)
}

fn merge_intervals(intervals: Vec<(usize, usize)>) -> Vec<(usize, usize)> {
    let mut stack: VecDeque<(usize, usize)> = VecDeque::new();
    for curr @ (curr_left, curr_right) in intervals {
        match stack.back() {
            None => stack.push_back(curr),
            Some(&(prev_left, prev_right)) =>
                if curr_left <= prev_right + 1 {
                    stack.pop_back();
                    stack.push_back((prev_left, curr_right.max(prev_right)))
                } else {
                    stack.push_back(curr)
                }
        }
    }
    stack.into_iter().collect_vec()
}

fn parse_input(input: Vec<String>) -> Vec<(usize, usize)> {
    input.iter().map(|line| {
        let parts = line.split('-').collect_vec();
        (parts[0].parse().unwrap(), parts[1].parse().unwrap())
    }).sorted().collect_vec()
}
