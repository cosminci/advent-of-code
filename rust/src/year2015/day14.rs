use itertools::{FoldWhile::{Continue, Done}, Itertools};

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2015/resources/day14.txt");
    let reindeers = parse_input(input);

    println!("Part 1: {}", longest_distance(&reindeers, 2503));
    println!("Part 2: {}", biggest_score(&reindeers, 2503));
}

fn biggest_score(reindeers: &Vec<(u16, u16, u16)>, time: u16) -> u16 {
    let stats = reindeers.iter().map(|(_, ttl, _)| (0_u16, *ttl, false)).collect_vec();
    let scores = reindeers.iter().map(|_| 0_u16).collect_vec();

    let final_stats = (0..time)
        .fold((scores, stats), |(scores, stats), _| {
            let new_stats = stats.iter().enumerate()
                .map(|(idx, &(distance, ttl, resting))| {
                    if resting && ttl == 0 {
                        (distance + reindeers[idx].0, reindeers[idx].1 - 1, false)
                    } else if resting {
                        (distance, ttl - 1, true)
                    } else if ttl == 0 {
                        (distance, reindeers[idx].2 - 1, true)
                    } else {
                        (distance + reindeers[idx].0, ttl - 1, false)
                    }
                }).collect_vec();

            let max_distance = new_stats.iter().map(|&s| s.0).max().unwrap();
            let new_scores = scores.iter().enumerate().map(|(idx, score)| {
                score + if new_stats[idx].0 == max_distance { 1 } else { 0 }
            }).collect_vec();

            (new_scores, new_stats)
        });
    final_stats.0.into_iter().max().unwrap()
}

fn longest_distance(reindeers: &Vec<(u16, u16, u16)>, time: u16) -> u16 {
    reindeers.iter().map(|&r| distance_travelled_after(r, time)).max().unwrap()
}

fn distance_travelled_after(reindeer: (u16, u16, u16), time: u16) -> u16 {
    (0..).fold_while((0, 0, reindeer.1), |(ts, d, ttl), _| {
        if ts > time { Done((ts, d, ttl)) } else {
            Continue(
                if ttl == 0 { (ts + reindeer.2, d, reindeer.1) } else { (ts + 1, d + reindeer.0, ttl - 1) }
            )
        }
    }).into_inner().1
}

fn parse_input(input: Vec<String>) -> Vec<(u16, u16, u16)> {
    input.iter().map(|line| {
        let parts = line.split(" ").collect_vec();
        (
            parts[3].parse().unwrap(),
            parts[6].parse().unwrap(),
            parts[13].parse().unwrap()
        )
    }).collect_vec()
}
