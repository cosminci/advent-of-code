use std::borrow::Borrow;
use std::collections::HashMap;

use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2015/resources/day13.txt");
    let mut affinities = parse_input(input);

    println!("Part 1: {}", max_happiness(&affinities));
    add_self(&mut affinities);
    println!("Part 2: {}", max_happiness(&affinities));
}

fn add_self(affinities: &mut HashMap<String, HashMap<String, i8>>) {
    let self_affinities = affinities.keys().map(|p| (p.clone(), 0)).collect();
    affinities.insert(String::from("Cosmin"), self_affinities);
    affinities.values_mut().for_each(|affinity_group| {
        affinity_group.insert(String::from("Cosmin"), 0);
    })
}

fn max_happiness(affinities: &HashMap<String, HashMap<String, i8>>) -> i16 {
    affinities.keys()
        .permutations(affinities.len())
        .map(|arrangement| find_happiness(arrangement, &affinities))
        .max().unwrap()
}

fn find_happiness(arrangement: Vec<&String>, affinities: &HashMap<String, HashMap<String, i8>>) -> i16 {
    arrangement.iter().circular_tuple_windows().map(|(&p1, &p2, &p3)|
        *affinities.get(p2).unwrap().get(p1).unwrap() as i16 +
            *affinities.get(p2).unwrap().get(p3).unwrap() as i16
    ).sum::<i16>()
}

fn parse_input(input: Vec<String>) -> HashMap<String, HashMap<String, i8>> {
    input.into_iter()
        .map(|line| parse_line(line))
        .group_by(|(p1, _, _)| p1.clone())
        .borrow()
        .into_iter()
        .map(|(person, affinity_group)|
            (person, parse_affinities(affinity_group.collect_vec()))
        )
        .collect::<HashMap<String, HashMap<String, i8>>>()
}

fn parse_line(line: String) -> (String, String, i8) {
    let parts = line.split(' ').collect_vec();
    let p1 = parts[0].to_string();
    let p2 = parts[10].to_string();
    let sign = parts[2];
    let val = parts[3].parse::<i8>().unwrap();
    (p1, p2, if sign == "gain" { val } else { -val })
}

fn parse_affinities(affinity_group: Vec<(String, String, i8)>) -> HashMap<String, i8> {
    affinity_group.iter().map(|(_, p, affinity)| (p.clone(), *affinity)).collect()
}
