use std::collections::HashMap;

use itertools::Itertools;
use regex::Regex;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2015/resources/day16.txt");

    let belongings = parse_input(input);
    let aunts = belongings.keys().map(|&a| a).collect_vec();
    let criteria = HashMap::from([
        ("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0),
        ("vizslas", 0), ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)
    ]);

    println!("Part 1: {:?}", matching_aunts(&aunts, &belongings, &criteria, exact_count_predicate));
    println!("Part 2: {:?}", matching_aunts(&aunts, &belongings, &criteria, le_gt_count_predicate));
}

fn matching_aunts(
    aunts: &Vec<u16>,
    belongings: &HashMap<u16, HashMap<String, u8>>,
    criteria: &HashMap<&str, u8>,
    predicate: fn(&str, u8, u8) -> bool,
) -> Vec<u16> {
    criteria.into_iter().fold(aunts.clone(), |candidates, (&item, &expected)| {
        candidates.into_iter().filter(|a| {
            match belongings[a].get(item) {
                None => true,
                Some(&owned) => predicate(item, owned, expected)
            }
        }).collect_vec()
    })
}

fn exact_count_predicate(_: &str, owned: u8, expected: u8) -> bool {
    owned == expected
}

fn le_gt_count_predicate(item: &str, owned: u8, expected: u8) -> bool {
    if ["cats", "trees"].contains(&item) {
        expected < owned
    } else if ["pomeranians", "goldfish"].contains(&item) {
        expected > owned
    } else {
        expected == owned
    }
}

fn parse_input(input: Vec<String>) -> HashMap<u16, HashMap<String, u8>> {
    let aunt_prefix = Regex::new(r"^Sue (\d+): ").unwrap();

    input.into_iter().map(|line| {
        let caps = aunt_prefix.captures(line.as_str()).unwrap();
        let aunt = caps[1].parse().unwrap();
        let belongings_str = aunt_prefix.replace(line.as_str(), "");
        let belongings = belongings_str.split(", ").collect_vec();

        let counts = belongings.into_iter().map(|belonging| {
            let parts = belonging.split(": ").collect_vec();
            (parts[0].to_string(), parts[1].parse().unwrap())
        }).collect();

        (aunt, counts)
    }).collect()
}
