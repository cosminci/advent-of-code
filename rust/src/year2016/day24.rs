use std::collections::{HashMap, HashSet, VecDeque};

use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day24.txt");
    let grid = input.iter().map(|line| line.chars().collect()).collect();

    println!("Part 1: {}", tsp(&grid, false));
    println!("Part 2: {}", tsp(&grid, true));
}

type Pos = (usize, usize);
type Grid = Vec<Vec<char>>;

// Brute force Travelling Salesman Problem
fn tsp(grid: &Grid, return_to_start: bool) -> usize {
    let nums = find_nums(grid);
    let start = nums.iter().find(|(x, y)| grid[*x][*y] == '0').unwrap();
    let pairwise_distances = calculate_distances(grid, &nums);

    nums.iter()
        .filter(|(x, y)| grid[*x][*y] != '0')
        .permutations(nums.len() - 1)
        .map(|order| {
            let optional_return = if return_to_start { vec![start] } else { vec![] };
            let chain = [vec![start], order, optional_return].concat();
            (1..chain.len())
                .map(|i| pairwise_distances[chain[i - 1]][chain[i]])
                .sum::<usize>()
        })
        .min().unwrap()
}

fn calculate_distances(grid: &Grid, nums: &Vec<Pos>) -> HashMap<Pos, HashMap<Pos, usize>> {
    nums.iter()
        .combinations(2)
        .flat_map(|targets| {
            let (p1, p2) = (targets[0], targets[1]);
            let steps = bfs(p1, p2, &grid);
            vec![(*p1, (*p2, steps)), (*p2, (*p1, steps))]
        })
        .into_group_map_by(|(k, _)| *k).iter()
        .map(|(k, kvs)| (*k, kvs.iter().map(|(_, v)| *v).collect()))
        .collect()
}

fn bfs(start: &Pos, target: &Pos, grid: &Grid) -> usize {
    let mut to_visit = VecDeque::from([(*start, 0)]);
    let mut visited = HashSet::from([*start]);

    while let Some((p @ (x, y), steps)) = to_visit.pop_front() {
        if p == *target { return steps }

        for nei @ (nx, ny) in utils::neighbours(x, y, grid.len(), grid[0].len(), false) {
            if !visited.contains(&nei) && grid[nx][ny] != '#' {
                visited.insert(nei);
                to_visit.push_back((nei, steps + 1))
            }
        }
    }

    panic!("Could not find any path from {:?} to {:?}", start, target);
}

fn find_nums(grid: &Grid) -> Vec<Pos> {
    grid.iter().enumerate().flat_map(|(x, row)|
        row.iter().enumerate().filter_map(|(y, value)|
            match value.to_digit(10) {
                None => None,
                Some(_) => Some((x, y))
            }
        ).collect_vec()
    ).collect()
}
