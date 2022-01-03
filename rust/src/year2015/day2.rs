use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let box_sides = utils::read_lines("./src/year2015/resources/day2.txt").iter()
        .map(|line| line.split('x').map(|size| size.parse::<usize>().unwrap()).collect_vec())
        .map(|sizes| (sizes[0], sizes[1], sizes[2]))
        .collect_vec();
    println!("Part 1: {}", wrapping_paper_needed(&box_sides));
    println!("Part 2: {}", ribbon_needed(&box_sides));
}

fn wrapping_paper_needed(box_sizes: &Vec<(usize, usize, usize)>) -> usize {
    box_sizes.iter()
        .map(|(x, y, z)| (x * y, x * z, y * z))
        .fold(0, |total, (a, b, c)| total + a * 2 + b * 2 + c * 2 + a.min(b).min(c))
}

fn ribbon_needed(box_sizes: &Vec<(usize, usize, usize)>) -> usize {
    box_sizes.iter().fold(0, |total, (x, y, z)| {
        total + [x, y, z].into_iter().sorted().take(2).map(|side| side * 2).sum::<usize>() + x * y * z
    })
}
