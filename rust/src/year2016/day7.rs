use itertools::{Either, Itertools};
use regex::Regex;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day7.txt");
    let ips = input.iter().map(|line| parse_ip(line)).collect_vec();

    println!("Part 1: {}", count_tls_ips(&ips));
    println!("Part 2: {}", count_ssl_ips(&ips));
}

fn count_tls_ips(ips: &Vec<(Vec<&str>, Vec<&str>)>) -> usize {
    ips.iter().filter(|(supernets, hypernets)|
        contains_abba(supernets) && !contains_abba(hypernets)
    ).count()
}

fn count_ssl_ips(ips: &Vec<(Vec<&str>, Vec<&str>)>) -> usize {
    ips.iter().filter(|(supernets, hypernets)|
        aba_sequences(supernets).iter().any(|(first, second)| {
            let bab = format!("{}{}{}", second, first, second);
            hypernets.iter().any(|hypernet|
                hypernet.contains(bab.as_str())
            )
        })
    ).count()
}

fn parse_ip(ip: &String) -> (Vec<&str>, Vec<&str>) {
    Regex::new("[\\[\\]]").unwrap()
        .split(ip)
        .collect_vec()
        .iter()
        .enumerate()
        .partition_map(|(idx, part)|
            if idx % 2 == 0 { Either::Left(part) } else { Either::Right(part) }
        )
}

fn contains_abba(seqs: &Vec<&str>) -> bool {
    seqs.iter().any(|seq|
        (0..seq.len() - 3).any(|i|
            seq.chars().nth(i) != seq.chars().nth(i + 1) &&
                seq.chars().nth(i) == seq.chars().nth(i + 3) &&
                seq.chars().nth(i + 1) == seq.chars().nth(i + 2))
    )
}

fn aba_sequences(seq: &Vec<&str>) -> Vec<(char, char)> {
    seq.iter().flat_map(|seq|
        (0..seq.len() - 2).filter_map(|i| {
            let first = seq.chars().nth(i).unwrap();
            let second = seq.chars().nth(i + 1).unwrap();
            let third = seq.chars().nth(i + 2).unwrap();
            if first != second && first == third { Some((first, second)) } else { None }
        })
    ).collect_vec()
}
