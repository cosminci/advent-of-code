use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::collections::HashMap;

use itertools::Itertools;
use regex::{Captures, Regex};

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day10.txt");
    let (mut state, transfer_fns) = parse_input(input);

    println!("Part 1: {}", simulate(&mut state, &transfer_fns, find_bot_comparing));
    println!("Part 2: {}", simulate(&mut state, &transfer_fns, multiply_outputs));
}

#[derive(Copy, Clone, Eq, Hash, PartialEq)]
enum Holder {
    Bot(u8),
    Output(u8),
}

fn simulate<F: Fn(&HashMap<Holder, Vec<u8>>) -> Option<u32>>(
    state: &mut HashMap<Holder, Vec<u8>>,
    transfer_fns: &HashMap<Holder, (Holder, Holder)>,
    stop_fn: F,
) -> u32 {
    loop {
        match stop_fn(state) {
            Some(result) => return result,
            None => {
                for bot in transferring_bots(state) {
                    let values = &state[&bot];
                    let low_value = values[0].min(values[1]);
                    let high_value = values[0].max(values[1]);

                    let (low_dest, high_dest) = &transfer_fns[&bot];
                    transfer(state, low_value, low_dest);
                    transfer(state, high_value, high_dest);
                    state.get_mut(&bot).unwrap().clear();
                };
            }
        }
    }
}

fn find_bot_comparing(state: &HashMap<Holder, Vec<u8>>) -> Option<u32> {
    let (v1, v2) = (17, 61);
    let maybe_bot = state.iter().find(|(_, values)| {
        values.contains(&v1) && values.contains(&v2)
    });
    match maybe_bot {
        Some((Holder::Bot(bot_id), _)) => Some(*bot_id as u32),
        _ => None
    }
}

fn multiply_outputs(state: &HashMap<Holder, Vec<u8>>) -> Option<u32> {
    let output_ids = vec![0, 1, 2];
    output_ids.iter()
        .fold(Some(1), |result, o| {
            match (result, state.get(&Holder::Output(*o))) {
                (None, _) | (_, None) => None,
                (Some(p), Some(v)) => {
                    let value = *v.first().unwrap() as u32;
                    Some(p * value)
                }
            }
        })
}

fn transferring_bots(state: &HashMap<Holder, Vec<u8>>) -> Vec<Holder> {
    state.into_iter().filter_map(|(holder, values)| {
        match holder {
            Holder::Output(_) => None,
            bot => if values.len() < 2 { None } else { Some(bot.clone()) }
        }
    }).collect_vec()
}

fn transfer(state: &mut HashMap<Holder, Vec<u8>>, value: u8, dest: &Holder) {
    match state.entry(*dest) {
        Vacant(entry) => entry.insert(Vec::new()),
        Occupied(entry) => entry.into_mut(),
    }.push(value);
}

fn parse_input(input: Vec<String>) -> (HashMap<Holder, Vec<u8>>, HashMap<Holder, (Holder, Holder)>) {
    let transfer_instructions = parse_transfer_functions(&input);
    let init_instructions = parse_init_instructions(input);

    (init_instructions, transfer_instructions)
}

fn parse_init_instructions(input: Vec<String>) -> HashMap<Holder, Vec<u8>> {
    let initialize_regex = Regex::new("value ([0-9]+) goes to bot ([0-9]+)").unwrap();
    input.iter().filter_map(|line| {
        initialize_regex
            .captures(line)
            .map(|captures| parse_init_instruction(captures))
    }).into_group_map_by(|&(id, _)| Holder::Bot(id))
        .into_iter()
        .map(|(k, kvs)| (k, kvs.into_iter().map(|(_, v)| v).collect()))
        .collect()
}

fn parse_init_instruction(captures: Captures) -> (u8, u8) {
    (captures[2].parse().unwrap(), captures[1].parse().unwrap())
}

fn parse_transfer_functions(input: &Vec<String>) -> HashMap<Holder, (Holder, Holder)> {
    let transfer_regex = Regex::new("bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (bot|output) ([0-9]+)").unwrap();
    input.iter().filter_map(|line| {
        transfer_regex
            .captures(line)
            .map(|captures| parse_transfer_function(captures))
    }).collect()
}

fn parse_transfer_function(captures: Captures) -> (Holder, (Holder, Holder)) {
    let source = Holder::Bot(captures[1].parse().unwrap());
    let dest_low = parse_destination(&captures[3], &captures[2]);
    let dest_high = parse_destination(&captures[5], &captures[4]);
    (source, (dest_low, dest_high))
}

fn parse_destination(id_str: &str, kind_str: &str) -> Holder {
    let id = id_str.parse().unwrap();
    if kind_str == "bot" { Holder::Bot(id) } else { Holder::Output(id) }
}
