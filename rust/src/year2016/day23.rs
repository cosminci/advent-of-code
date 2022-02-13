use im_rc::{hashmap, HashMap};
use itertools::Itertools;
use tailcall::tailcall;

use crate::utils;
use crate::year2016::day23::Instruction::*;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day23.txt");
    let mut instructions = parse_input(input);

    let registers_part_1 = hashmap! { 'a' => 7, 'b' => 0, 'c' => 0, 'd' => 0 };
    println!("Part 1: {}", execute(&mut instructions.clone(), registers_part_1, 0)[&'a']);
    let registers_part_2 = hashmap! { 'a' => 12, 'b' => 0, 'c' => 0, 'd' => 0 };
    println!("Part 2: {}", execute(&mut instructions, registers_part_2, 0)[&'a']);
}

#[derive(Clone)]
enum Instruction {
    Copy(Source, char),
    Inc(char),
    Dec(char),
    Add(char, char),
    Mul(char, char),
    Jnz(Source, Source),
    Tgl(char),
    Invalid,
}

#[derive(Clone)]
enum Source {
    Register(char),
    Value(i32),
}

#[tailcall]
fn execute(instr: &mut Vec<Instruction>, registers: HashMap<char, i32>, curr_idx: i32) -> HashMap<char, i32> {
    if curr_idx < 0 || curr_idx as usize >= instr.len() { return registers }

    let (updated_registers, next_idx) = match instr[curr_idx as usize].clone() {
        Invalid => (registers, curr_idx + 1),
        Inc(r) => (registers.update(r, registers[&r] + 1), curr_idx + 1),
        Dec(r) => (registers.update(r, registers[&r] - 1), curr_idx + 1),
        Add(src, dest) =>
            (registers.update(dest, registers[&dest] + registers[&src]), curr_idx + 1),
        Mul(src, dest) =>
            (registers.update(dest, registers[&dest] * registers[&src]), curr_idx + 1),
        Copy(src, dest) =>
            (registers.update(dest, src_value(&registers, &src)), curr_idx + 1),
        Jnz(src, jump) =>
            if src_value(&registers, &src) != 0 {
                let next_idx = curr_idx + src_value(&registers, &jump);
                (registers, next_idx)
            } else { (registers, curr_idx + 1) }
        Tgl(r) => {
            toggle(instr, curr_idx + registers[&r]);
            (registers, curr_idx + 1)
        }
    };

    execute(instr, updated_registers, next_idx)
}

fn toggle(instr: &mut Vec<Instruction>, idx: i32) {
    if idx >= 0 && (idx as usize) < instr.len() {
        instr[idx as usize] = match &instr[idx as usize] {
            Invalid | Add(_, _) | Mul(_, _) => Invalid,
            Inc(r) => Dec(*r),
            Dec(r) => Inc(*r),
            Copy(src, dest) => Jnz(src.clone(), Source::Register(*dest)),
            Jnz(src, jump) => match jump {
                Source::Register(r) => Copy(src.clone(), *r),
                Source::Value(_) => Invalid
            },
            Tgl(r) => Inc(*r)
        }
    }
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
            "add" => Add(parse_register(parts[1]), parse_register(parts[2])),
            "mul" => Mul(parse_register(parts[1]), parse_register(parts[2])),
            "jnz" => Jnz(parse_source(parts[1]), parse_source(parts[2])),
            _ => Tgl(parse_register(parts[1]))
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
