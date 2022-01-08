use std::borrow::Borrow;

use itertools::Itertools;

pub fn solve() {
    let first_pw = "hxbxwxba";
    println!("Part 1: {}", next_password(first_pw.to_string()));
    println!("Part 2: {}", next_password(next_password(first_pw.to_string())));
}

fn next_password(curr: String) -> String {
    let curr_decoded = decode(curr.as_str());
    (1..).map(|inc| encode(curr_decoded + inc))
        .find(|next| is_valid(next)).unwrap()
}

fn decode(s: &str) -> usize {
    s.bytes().fold(0, |v, b| v * 26 + (b - b'a') as usize)
}

fn encode(v: usize) -> String {
    fn dfs(v: usize, r: String) -> String {
        if v == 0 {
            format!("{:a>8}", r)
        } else {
            dfs(v / 26, format!("{}{}", ((b'a' + (v % 26) as u8) as char), r))
        }
    }
    dfs(v, String::new())
}

fn is_valid(s: &str) -> bool {
    has_increasing_straight(s) && has_no_invalid_chars(s) && has_min_two_overlapping_pairs(s)
}

fn has_increasing_straight(s: &str) -> bool {
    s.chars().tuple_windows().any(|(a, b, c)|
        a as usize + 1 == b as usize && b as usize + 1 == c as usize
    )
}

fn has_no_invalid_chars(s: &str) -> bool {
    s.chars().all(|c| c != 'i' && c != 'o' && c != 'l')
}

fn has_min_two_overlapping_pairs(s: &str) -> bool {
    s.chars()
        .group_by(|&c| c).borrow().into_iter()
        .map(|(_, group)| group.collect_vec().len() / 2)
        .sum::<usize>() >= 2
}