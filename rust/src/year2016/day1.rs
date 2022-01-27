use std::collections::HashSet;
use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;
use crate::utils;

pub fn solve() {
    let input = utils::read_first_line("./src/year2016/resources/day1.txt");
    let directions = parse_input(&input);

    println!("Part 1: {}", distance_to_destination(&directions));
    println!("Part 2: {}", distance_to_first_revisited_location(&directions));
}

fn distance_to_destination(directions: &Vec<(char, u16)>) -> u16 {
    let (x, y, _) = directions.iter()
        .fold((0_i16, 0_i16, 0), |(x, y, dir), &(turn, steps)| {
            update_position(x, y, dir, turn, steps)
        });
    x.abs() as u16 + y.abs() as u16
}

fn distance_to_first_revisited_location(directions: &Vec<(char, u16)>) -> u16 {
    let mut seen = HashSet::<(i16, i16)>::new();
    let (x, y, _) = directions.iter()
        .fold_while((0_i16, 0_i16, 0), |(x, y, dir), &(turn, steps)| {
            let (nx, ny, nd) = update_position(x, y, dir, turn, steps);

            let path = if x == nx {
                get_path(y, ny).into_iter().map(|y| (x, y)).collect_vec()
            } else {
                get_path(x, nx).into_iter().map(|x| (x, y)).collect_vec()
            };

            match path.into_iter().find(|&xy| !seen.insert(xy)) {
                Some((x, y)) => Done((x, y, nd)),
                None => Continue((nx, ny, nd))
            }
        }).into_inner();
    x.abs() as u16 + y.abs() as u16
}

fn get_path(c: i16, dc: i16) -> Vec<i16> {
    if c > dc { (dc..=c - 1).rev().collect_vec() } else { (c + 1..=dc).collect_vec() }
}

fn update_position(x: i16, y: i16, dir: i32, turn: char, steps: u16) -> (i16, i16, i32) {
    let new_dir = if turn == 'R' { (dir + 1) % 4 } else { (dir + 3) % 4 };
    let (new_x, new_y) = match new_dir {
        0 => (x, y + steps as i16),
        1 => (x + steps as i16, y),
        2 => (x, y - steps as i16),
        _ => (x - steps as i16, y)
    };
    (new_x, new_y, new_dir)
}

fn parse_input(input: &String) -> Vec<(char, u16)> {
    input.split(", ").map(|s|
        (s.chars().nth(0).unwrap(),
         s.chars().dropping(1).as_str().parse().unwrap())
    ).collect_vec()
}
