use boolinator::Boolinator;
use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2015/resources/day23.txt");
    let instructions = parse_input(input);

    println!("Part 1: {}", execute(&instructions, 0, [0, 0])[1]);
    println!("Part 1: {}", execute(&instructions, 0, [1, 0])[1]);
}

fn execute(instructions: &Vec<Instruction>, mut index: isize, mut registers: [usize; 2]) -> [usize; 2] {
    while index >= 0 && index < instructions.len() as isize {
        match instructions[index as usize] {
            Instruction::Inc(r) => registers[r] += 1,
            Instruction::Hlf(r) => registers[r] /= 2,
            Instruction::Tpl(r) => registers[r] *= 3,
            Instruction::Jmp(i) => index += i - 1,
            Instruction::Jio(r, i) =>
                index += (registers[r] == 1).as_some(i - 1).unwrap_or(0),
            Instruction::Jie(r, i) => {
                index += (registers[r] % 2 == 0).as_some(i - 1).unwrap_or(0)
            }
        }
        index += 1;
    }
    registers
}

fn parse_input(input: Vec<String>) -> Vec<Instruction> {
    input.iter().map(|line| {
        let parts = line.split(' ').collect_vec();
        match parts[0] {
            "inc" => Instruction::Inc(parse_register(&parts)),
            "tpl" => Instruction::Tpl(parse_register(&parts)),
            "hlf" => Instruction::Hlf(parse_register(&parts)),
            "jmp" => Instruction::Jmp(parts[1].parse().unwrap()),
            "jio" => Instruction::Jio(parse_register(&parts), parts[2].parse().unwrap()),
            _ => Instruction::Jie(parse_register(&parts), parts[2].parse().unwrap())
        }
    }).collect_vec()
}

fn parse_register(parts: &Vec<&str>) -> usize {
    (parts[1].starts_with('a')).as_some(0).unwrap_or(1)
}

enum Instruction {
    Inc(usize),
    Tpl(usize),
    Hlf(usize),
    Jmp(isize),
    Jio(usize, isize),
    Jie(usize, isize),
}
