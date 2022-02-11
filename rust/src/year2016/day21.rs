use itertools::Itertools;

use Instruction::*;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day21.txt");
    let instructions = parse_input(input);

    println!("Part 1: {}", scramble(String::from("abcdefgh"), &instructions));
    println!("Part 2: {}", unscramble(String::from("fbgdceah"), &instructions));
}

fn scramble(s: String, instructions: &Vec<Instruction>) -> String {
    let mut pw = s.chars().collect_vec();
    for i in instructions {
        match i {
            SwapPositions(x, y) => pw.swap(*x, *y),
            SwapLetters(a, b) => {
                let x = index_of(&pw, *a);
                let y = index_of(&pw, *b);
                pw.swap(x, y);
            },
            RotateLeft(x) => pw.rotate_left(*x),
            RotateRight(x) => pw.rotate_right(*x),
            RotateRelative(a) => {
                let mut x = 1 + index_of(&pw, *a);
                if x > 4 { x += 1 };
                pw.rotate_right(x % s.len())
            },
            Reverse(x, y) => {
                let tmp = pw.clone();
                for i in *x..=*y {
                    pw[i] = tmp[y + x - i];
                };
            },
            Move(x, y) => {
                let a = pw.remove(*x);
                pw.insert(*y, a)
            }
        }
    }
    pw.iter().collect()
}

fn unscramble(s: String, instructions: &Vec<Instruction>) -> String {
    s.chars()
        .permutations(s.len())
        .find(|p| scramble(p.iter().collect(), instructions) == s)
        .unwrap()
        .iter()
        .collect()
}

fn index_of(pw: &Vec<char>, a: char) -> usize {
    (0..pw.len()).find(|i| pw[*i] == a).unwrap()
}

enum Instruction {
    SwapPositions(usize, usize),
    SwapLetters(char, char),
    RotateLeft(usize),
    RotateRight(usize),
    RotateRelative(char),
    Reverse(usize, usize),
    Move(usize, usize),
}

fn parse_input(input: Vec<String>) -> Vec<Instruction> {
    input.iter().map(|line| {
        let parts = line.split(' ').collect_vec();
        if line.starts_with("swap p") {
            SwapPositions(parts[2].parse().unwrap(), parts[5].parse().unwrap())
        } else if line.starts_with("swap l") {
            SwapLetters(parts[2].chars().nth(0).unwrap(), parts[5].chars().nth(0).unwrap())
        } else if line.starts_with("rotate l") {
            RotateLeft(parts[2].parse().unwrap())
        } else if line.starts_with("rotate r") {
            RotateRight(parts[2].parse().unwrap())
        } else if line.starts_with("rotate based") {
            RotateRelative(parts[6].chars().nth(0).unwrap())
        } else if line.starts_with("reverse") {
            Reverse(parts[2].parse().unwrap(), parts[4].parse().unwrap())
        } else {
            Move(parts[2].parse().unwrap(), parts[5].parse().unwrap())
        }
    }).collect_vec()
}
