use std::collections::HashSet;

use itertools::Itertools;

pub fn solve() {
    let door_id = "wtnhxymk";

    println!("Part 1: {}", discover_password_1(door_id));
    println!("Part 2: {}", discover_password_2(door_id));
}

fn discover_password_1(door_id: &str) -> String {
    let predicate = |_: &String| true;
    discover_hashes(door_id, predicate)
        .iter()
        .map(|hash| hash.chars().nth(5).unwrap())
        .collect()
}

fn discover_password_2(door_id: &str) -> String {
    let mut seen = HashSet::new();
    let predicate = |h: &String| {
        let nth = h.chars().nth(5).unwrap();
        nth >= '0' && nth <= '7' && seen.insert(nth)
    };
    discover_hashes(door_id, predicate)
        .iter()
        .map(|hash| (hash.chars().nth(5).unwrap(), hash.chars().nth(6).unwrap()))
        .sorted_by_key(|&(idx, _)| idx)
        .map(|(_, value)| value)
        .collect()
}

fn discover_hashes<F: FnMut(&String) -> bool>(door_id: &str, mut predicate: F) -> Vec<String> {
    (1..).filter_map(|i| {
        let hash = format!("{:x}", md5::compute(format!("{}{}", door_id, i)));
        if hash.starts_with("00000") && predicate(&hash) { Some(hash) } else { None }
    }).take(8).collect()
}
