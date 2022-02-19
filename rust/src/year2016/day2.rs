use std::collections::HashMap;

use crate::utils;

pub fn solve() {
    let instructions = utils::read_lines("./src/year2016/resources/day2.txt");

    println!("Part 1: {}", access_code_keypad_1(&instructions));
    println!("Part 2: {}", access_code_keypad_2(&instructions));
}

fn access_code_keypad_1(instructions: &Vec<String>) -> u32 {
    instructions.iter().fold((0, 1, 1), |(code, x, y), str| {
        let (nx, ny) = str.chars().fold((x, y), |(x, y), char| {
            let (nx, ny) = new_position(x, y, char);
            if nx < 0 || nx > 2 || ny < 0 || ny > 2 { (x, y) } else { (nx, ny) }
        });
        let digit = (nx * 3 + ny + 1) as u32;
        (code * 10 + digit, nx, ny)
    }).0
}

fn access_code_keypad_2(instructions: &Vec<String>) -> String {
    let keypad = HashMap::from([
        ((0, 2), '1'),
        ((1, 1), '2'), ((1, 2), '3'), ((1, 3), '4'),
        ((2, 0), '5'), ((2, 1), '6'), ((2, 2), '7'), ((2, 3), '8'), ((2, 4), '9'),
        ((3, 1), 'A'), ((3, 2), 'B'), ((3, 3), 'C'),
        ((4, 2), 'D')
    ]);
    instructions.iter().fold((String::new(), 2, 0), |(code, x, y), str| {
        let (nx, ny) = str.chars().fold((x, y), |(x, y), char| {
            let (nx, ny) = new_position(x, y, char);
            if keypad.contains_key(&(nx, ny)) { (nx, ny) } else { (x, y) }
        });
        (format!("{}{}", code, keypad[&(nx, ny)]), nx, ny)
    }).0
}

fn new_position(x: i32, y: i32, char: char) -> (i32, i32) {
    let (nx, ny) = match char {
        'D' => (x + 1, y),
        'U' => (x - 1, y),
        'L' => (x, y - 1),
        _ => (x, y + 1)
    };
    (nx, ny)
}
