use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let input = utils::read_first_line("./src/year2016/resources/day18.txt");
    let first_row = parse_input(input);

    println!("Part 1: {}", count_total_safe_tiles(&first_row, 40));
    println!("Part 2: {}", count_total_safe_tiles(&first_row, 400000));
}

fn count_total_safe_tiles(first_row: &Vec<bool>, total_rows: usize) -> usize {
    let initial_count = count_safe_tiles(&first_row);
    (1..total_rows).fold((first_row.clone(), initial_count), |(prev_row, count), _| {
        let new_row = discover_next_row(&prev_row);
        let new_count = count + count_safe_tiles(&new_row);
        (new_row, new_count)
    }).1
}

fn count_safe_tiles(row: &Vec<bool>) -> usize {
    row.iter().filter(|v| !**v).count()
}

fn discover_next_row(row: &Vec<bool>) -> Vec<bool> {
    (0..row.len()).map(|i| {
        let left = if i > 0 { row[i - 1] } else { false };
        let right = if i < row.len() - 1 { row[i + 1] } else { false };
        left ^ right
    }).collect_vec()
}

fn parse_input(input: String) -> Vec<bool> {
    input.chars()
        .map(|c|
            if c == '^' { true } else { false }
        )
        .collect_vec()
}
