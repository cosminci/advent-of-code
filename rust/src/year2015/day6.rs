use std::ops::Range;

use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let instructions = utils::read_lines("./src/year2015/resources/day6.txt")
        .into_iter()
        .map(|line| {
            let parts = line.split(' ').collect_vec();
            let from = parts[1].split(',').map(|s| s.parse::<usize>().unwrap()).collect_vec();
            let to = parts[3].split(',').map(|s| s.parse::<usize>().unwrap()).collect_vec();
            (parts[0].to_string(), from[0]..to[0] + 1, from[1]..to[1] + 1)
        }).collect::<Vec<(String, Range<usize>, Range<usize>)>>();

    println!("Part 1: {}", lights_on(&instructions));
    println!("Part 2: {}", total_brightness(&instructions));
}

fn lights_on(instructions: &Vec<(String, Range<usize>, Range<usize>)>) -> usize {
    let mut grid = [[false; 1000]; 1000];
    instructions.into_iter().for_each(|(cmd, xr, yr)| {
        xr.clone().for_each(|x| {
            yr.clone().for_each(|y| {
                grid[x][y] = match cmd.as_str() {
                    "turn_on" => true,
                    "turn_off" => false,
                    _ => !grid[x][y]
                };
            })
        })
    });
    grid.iter().map(|row| row.iter().filter(|&&col| col).count()).sum()
}

fn total_brightness(instructions: &Vec<(String, Range<usize>, Range<usize>)>) -> usize {
    let mut grid = [[0_usize; 1000]; 1000];
    instructions.into_iter().for_each(|(cmd, xr, yr)| {
        xr.clone().for_each(|x| {
            yr.clone().for_each(|y| {
                grid[x][y] = match cmd.as_str() {
                    "turn_on" => grid[x][y] + 1,
                    "turn_off" => grid[x][y].saturating_sub(1),
                    _ => grid[x][y] + 2
                };
            })
        })
    });
    grid.iter().map(|row| row.iter().sum::<usize>()).sum()
}
