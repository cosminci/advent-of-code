use itertools::Itertools;
use regex::Regex;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day4.txt");
    let rooms = valid_rooms(parse_input(input));

    println!("Part 1: {}", count_valid_rooms(&rooms));
    println!("Part 2: {}", north_pole_room(&rooms));
}

fn count_valid_rooms(rooms: &Vec<(String, u32, String)>) -> u32 {
    rooms.iter().map(|(_, sector_id, _)| sector_id).sum()
}

fn north_pole_room(rooms: &Vec<(String, u32, String)>) -> u32 {
    *rooms.iter()
        .map(|(name, sector_id, _)| (shift(name, *sector_id), sector_id))
        .find(|(decrypted_name, _)| decrypted_name.starts_with("northpole"))
        .unwrap().1
}

fn shift(data: &String, shift: u32) -> String {
    let (a, z) = ('a' as u32, 'z' as u32);
    data.chars().map(|c| {
        if c == '-' { c } else {
            let ascii = c as u32 + shift % 26;
            let ascii = if ascii > z { ascii % (z + 1) + a } else { ascii };
            ascii as u8 as char
        }
    }).collect::<String>()
}

fn valid_rooms(rooms: Vec<(String, u32, String)>) -> Vec<(String, u32, String)> {
    rooms.into_iter().filter(|(name, _, checksum)| {
        name.replace("-", "")
            .chars()
            .into_group_map_by(|&c| c)
            .into_iter()
            .map(|(c, v)| (c, v.len() as isize))
            .sorted_by_key(|&(c, count)| (-count, c))
            .map(|(c, _)| c)
            .collect::<String>()
            .starts_with(checksum)
    }).collect_vec()
}

fn parse_input(input: Vec<String>) -> Vec<(String, u32, String)> {
    let re = Regex::new("([a-z-]+)-([0-9]+)\\[([a-z]+)]").unwrap();
    input.iter().map(|s| {
        let parts = re.captures(s).unwrap();
        (String::from(&parts[1]), parts[2].parse().unwrap(), String::from(&parts[3]))
    }).collect_vec()
}
