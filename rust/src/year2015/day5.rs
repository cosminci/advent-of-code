use std::collections::HashSet;
use itertools::Itertools;

use substring::Substring;

use crate::utils;

pub fn solve() {
    let strings = utils::read_lines("./src/year2015/resources/day5.txt");

    println!("Part 1: {}", count_nice_strings_1(&strings));
    println!("Part 2: {}", count_nice_strings_2(&strings));
}

fn count_nice_strings_1(strings: &Vec<String>) -> usize {
    let vowels = HashSet::from(['a', 'e', 'i', 'o', 'u']);
    let doubles = ('a'..='z').map(|c| format!("{}{}", c, c)).collect::<HashSet<String>>();
    let denied = HashSet::from(["ab", "cd", "pq", "xy"]);

    strings.iter().filter(|&s| {
        let vowel_count = s.chars().filter(|c| vowels.contains(c)).collect_vec().len();
        let double_exists = doubles.iter().any(|d| s.contains(d));
        let no_denied = denied.iter().all(|&d| !s.contains(d));
        vowel_count >= 3 && double_exists && no_denied
    }).count()
}

fn count_nice_strings_2(strings: &Vec<String>) -> usize {
    strings.iter().filter(|&s| {
        let pair_occurs_twice = (0..s.len() - 3).any(|start| {
            let pattern = s.substring(start, start + 2);
            s.substring(start + 2, s.len()).contains(pattern)
        });
        let distance_one_repeat = (0..s.len() - 2).any(|i|
            s.chars().nth(i) == s.chars().nth(i + 2));
        pair_occurs_twice && distance_one_repeat
    }).count()
}
