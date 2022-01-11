use std::borrow::Borrow;

use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let containers = utils::read_lines_as("./src/year2015/resources/day17.txt");

    println!("Part 1: {:?}", ways(&containers, 0, 0, 150).len());
    println!("Part 2: {:?}", ways_min_containers(&containers, 150));
}

fn ways(containers: &Vec<u8>, idx: usize, used: usize, water: i16) -> Vec<usize> {
    if water == 0 {
        vec![used]
    } else if water < 0 || idx == containers.len() {
        vec![]
    } else {
        let with = ways(containers, idx + 1, used + 1, water - containers[idx] as i16);
        let without = ways(containers, idx + 1, used, water);
        [with, without].concat()
    }
}

fn ways_min_containers(containers: &Vec<u8>, water: i16) -> usize {
    ways(containers, 0, 0, water).into_iter()
        .into_group_map_by(|&v| v).borrow().iter()
        .map(|(&count, ways)| (count, ways.len()))
        .min_by_key(|&(v, _)| v).unwrap().1
}
