use im_rc::{hashmap, HashMap};
use itertools::Itertools;
use tailcall::tailcall;

use crate::utils;
use crate::year2016::day25::Instruction::*;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day25.txt");
    let instructions = parse_input(input);

    println!("Part 1: {}", lowest_a_for_clock_signal(&instructions));
}

#[derive(Clone)]
enum Instruction {
    Copy(Source, char),
    Inc(char),
    Dec(char),
    Jnz(Source, i32),
    Out(char),
}

#[derive(Clone)]
enum Source {
    Register(char),
    Value(i32),
}

fn lowest_a_for_clock_signal(instructions: &Vec<Instruction>) -> usize {
    (1..).skip_while(|&a| {
        let registers = hashmap! { 'a' => a, 'b' => 0, 'c' => 0, 'd' => 0 };
        let transmissions = execute(instructions, registers, 0, vec![]);
        transmissions.len() < 10 ||
            transmissions.iter().enumerate().any(|(idx, v)| *v as usize != idx % 2)
    }).next().unwrap() as usize
}

#[allow(unreachable_code)] #[tailcall]
fn execute(
    instr: &Vec<Instruction>,
    registers: HashMap<char, i32>,
    curr_idx: i32,
    transmissions: Vec<i32>,
) -> Vec<i32> {
    if curr_idx < 0 || curr_idx as usize >= instr.len() || transmissions.len() == 10 {
        return transmissions
    }

    let (updated_registers, updated_transmissions, next_idx) = match instr[curr_idx as usize].clone() {
        Inc(r) => (registers.update(r, registers[&r] + 1), transmissions, curr_idx + 1),
        Dec(r) => (registers.update(r, registers[&r] - 1), transmissions, curr_idx + 1),
        Copy(src, dest) =>
            (registers.update(dest, src_value(&registers, &src)), transmissions, curr_idx + 1),
        Jnz(src, jump) => {
            let next_idx = curr_idx + if src_value(&registers, &src) != 0 { jump } else { 1 };
            (registers, transmissions, next_idx)
        },
        Out(r) => {
            let out_value = registers[&r];
            (registers, [transmissions, vec![out_value]].concat(), curr_idx + 1)
        }
    };
    execute(instr, updated_registers, next_idx, updated_transmissions)
}

fn src_value(registers: &HashMap<char, i32>, source: &Source) -> i32 {
    match source {
        Source::Register(r) => registers[r],
        Source::Value(v) => *v
    }
}

fn parse_input(input: Vec<String>) -> Vec<Instruction> {
    input.iter().map(|line| {
        let parts = line.split(' ').collect_vec();
        match parts[0] {
            "cpy" => Copy(parse_source(parts[1]), parse_register(parts[2])),
            "inc" => Inc(parse_register(parts[1])),
            "dec" => Dec(parse_register(parts[1])),
            "jnz" => Jnz(parse_source(parts[1]), parts[2].parse().unwrap()),
            _ => Out(parse_register(parts[1]))
        }
    }).collect_vec()
}

fn parse_source(s: &str) -> Source {
    match s.parse::<i32>() {
        Err(_) => Source::Register(parse_register(s)),
        Ok(n) => Source::Value(n)
    }
}

fn parse_register(s: &str) -> char {
    s.chars().nth(0).unwrap()
}
