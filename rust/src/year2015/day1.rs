use itertools::{FoldWhile::{Continue, Done}, Itertools};

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2015/resources/day1.txt").first().unwrap().to_string();
    println!("Part 1: {}", final_floor(&input));
    println!("Part 2: {}", time_to_basement(&input));
}

fn final_floor(instructions: &String) -> i32 {
    instructions.chars().fold(0, change_floor)
}

fn change_floor(floor: i32, instruction: char) -> i32 {
    if instruction == '(' { floor + 1 } else { floor - 1 }
}

fn time_to_basement(instructions: &String) -> i32 {
    instructions.chars().enumerate().fold_while(0_i32, |floor, (i, instruction)| {
        if floor < 0 { Done(i as i32) } else { Continue(change_floor(floor, instruction)) }
    }).into_inner()
}
