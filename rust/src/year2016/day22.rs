use std::collections::{HashSet, VecDeque};

use itertools::Itertools;
use regex::Regex;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day22.txt");
    let disks = parse_input(input);

    println!("Part 1: {}", count_viable_pairs(&disks));
    println!("Part 2: {}", min_steps_to_retrieve_payload(&disks));
}


#[derive(Clone)]
struct Disk {
    pos: Pos,
    used: usize,
    size: usize,
}

type Pos = (usize, usize);

fn count_viable_pairs(disks: &Vec<Disk>) -> usize {
    disks.iter()
        .permutations(2)
        .filter(|pair| pair[0].used > 0 && pair[0].used <= pair[1].size - pair[1].used)
        .count()
}

fn min_steps_to_retrieve_payload(disks: &Vec<Disk>) -> usize {
    let (max_row, max_col) = disks.iter().map(|node| node.pos).max().unwrap();
    let grid = disks
        .chunks(max_col + 1)
        .map(|c| Vec::from(c))
        .collect_vec();

    let empty_disk_pos = disks.iter().find(|node| node.used == 0 && node.size > 0).unwrap().pos;
    let dist_to_payload = bfs(&grid, empty_disk_pos, (max_row, 0));
    let dist_to_access_point = bfs(&grid, (max_row, 0), (0, 0));

    dist_to_payload + 5 * (dist_to_access_point - 1)
}

fn bfs(grid: &Vec<Vec<Disk>>, start: Pos, target: Pos) -> usize {
    let mut to_visit = VecDeque::from([(start, 0)]);
    let mut visited = HashSet::from([start]);

    while let Some((p @ (x, y), steps)) = to_visit.pop_front() {
        if p == target { return steps }

        for nei @ (nx, ny) in utils::neighbours(x, y, grid.len(), grid[0].len(), false) {
            if !visited.contains(&nei) && !is_large_disk(&grid[nx][ny]) {
                visited.insert(nei.clone());
                to_visit.push_back((nei.clone(), steps + 1))
            }
        }
    }

    panic!("Could not find any path from {:?} to {:?}", start, target);
}

fn is_large_disk(disk: &Disk) -> bool {
    disk.used as f32 / disk.size as f32 >= 0.95
}

fn parse_input(input: Vec<String>) -> Vec<Disk> {
    let re = Regex::new("/dev/grid/node-x([0-9]+)-y([0-9]+) +([0-9]+)T +([0-9]+)T +([0-9]+)T +([0-9]+)%").unwrap();
    input.iter().dropping(2).map(|line| {
        let captures = re.captures(line).unwrap();
        Disk {
            pos: (captures[1].parse::<usize>().unwrap(), captures[2].parse::<usize>().unwrap()),
            used: captures[4].parse().unwrap(),
            size: captures[3].parse().unwrap(),
        }
    }).collect()
}
