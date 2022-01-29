use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day6.txt");

    let row_len = input.first().unwrap().len();
    let codes = transpose(input, row_len);

    println!("Part 1: {}", decode(&codes, true));
    println!("Part 2: {}", decode(&codes, false));
}

fn decode(codes: &Vec<String>, rev: bool) -> String {
    codes.iter()
        .map(|code| code.chars()
            .into_group_map_by(|&c| c)
            .into_iter()
            .map(|(c, group)| (c, group.len() as i8))
            .sorted_by_key(|&(_, count)| if rev { -count } else { count })
            .map(|(c, _)| c)
            .nth(0)
            .unwrap())
        .collect()
}

fn transpose(input: Vec<String>, row_len: usize) -> Vec<String> {
    (0..row_len).map(|i|
        input.iter()
            .map(|line| line.chars().nth(i).unwrap())
            .collect()
    ).collect()
}
