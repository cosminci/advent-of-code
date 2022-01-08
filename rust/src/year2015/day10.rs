use std::borrow::Borrow;

use itertools::Itertools;

pub fn solve() {
    let seed = "1113222113";

    println!("Part 1: {}", play(seed, 40));
    println!("Part 2: {}", play(seed, 50));
}

fn play(seed: &str, turns: u8) -> usize {
    if turns == 0 {
        seed.len()
    } else {
        let next_seed = seed.chars()
            .group_by(|&v| v).borrow().into_iter()
            .map(|(v, c)| format!("{}{}", c.collect_vec().len(), v))
            .collect_vec().concat();
        play(next_seed.as_str(), turns - 1)
    }
}
