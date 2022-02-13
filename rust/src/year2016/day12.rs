use im_rc::{vector, Vector};
use itertools::Itertools;
use tailcall::tailcall;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day12.txt");
    let instructions = parse_input(input);

    println!("Part 1: {}", execute(&instructions, vector![0, 0, 0, 0], 0)[0]);
    println!("Part 2: {}", execute(&instructions, vector![0, 0, 1, 0], 0)[0]);
}

enum Instruction {
    Copy(Source, usize),
    Inc(usize),
    Dec(usize),
    Jnz(Source, i32),
}

enum Source {
    Register(usize),
    Value(i32),
}

#[tailcall]
fn execute(instr: &Vec<Instruction>, registers: Vector<i32>, curr_idx: i32) -> Vector<i32> {
    if curr_idx < 0 || curr_idx as usize >= instr.len() { return registers }

    let (updated_registers, next_idx) = match &instr[curr_idx as usize] {
        Instruction::Inc(r) =>
            (registers.update(*r, registers[*r] + 1), curr_idx + 1),
        Instruction::Dec(r) =>
            (registers.update(*r, registers[*r] - 1), curr_idx + 1),
        Instruction::Copy(src, dest) =>
            (registers.update(*dest, src_value(&registers, &src)), curr_idx + 1),
        Instruction::Jnz(source, jump) =>
            if src_value(&registers, source) != 0 { (registers, curr_idx + jump) }
            else { (registers, curr_idx + 1) }
    };

    execute(instr, updated_registers, next_idx)
}

fn src_value(registers: &Vector<i32>, source: &Source) -> i32 {
    match source {
        Source::Register(r) => registers[*r],
        Source::Value(v) => *v
    }
}

fn parse_input(input: Vec<String>) -> Vec<Instruction> {
    input.iter().map(|line| {
        let parts = line.split(' ').collect_vec();
        match parts[0] {
            "cpy" => Instruction::Copy(parse_source(parts[1]), parse_register(parts[2])),
            "inc" => Instruction::Inc(parse_register(parts[1])),
            "dec" => Instruction::Dec(parse_register(parts[1])),
            _ => Instruction::Jnz(parse_source(parts[1]), parts[2].parse().unwrap())
        }
    }).collect_vec()
}

fn parse_source(s: &str) -> Source {
    match s.parse::<i32>() {
        Err(_) => Source::Register(parse_register(s)),
        Ok(n) => Source::Value(n)
    }
}

fn parse_register(s: &str) -> usize {
    s.chars().nth(0).unwrap() as usize - 'a' as usize
}
