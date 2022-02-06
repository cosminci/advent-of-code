use std::collections::{HashSet, VecDeque};

use tailcall::tailcall;

use crate::utils;

pub fn solve() {
    println!("Part 1: {}", fewest_steps_to_reach(31, 39, 1352));
    println!("Part 2: {}", count_reachable_locations(1352));
}

fn fewest_steps_to_reach(target_x: usize, target_y: usize, fav_number: usize) -> usize {
    let mut to_visit = VecDeque::from([(1, 1, 0)]);
    let mut visited = HashSet::from([(1, 1)]);

    while let Some((x, y, steps)) = to_visit.pop_front() {
        if x == target_x && y == target_y { return steps }
        for (nx, ny) in utils::neighbours(x, y, usize::MAX / 3, usize::MAX / 3, false) {
            if is_open_space(nx, ny, fav_number) && !visited.contains(&(nx, ny)) {
                visited.insert((nx, ny));
                to_visit.push_back((nx, ny, steps + 1));
            }
        }
    }

    usize::MAX
}

fn count_reachable_locations(fav_number: usize) -> usize {
    let mut to_visit = VecDeque::from([(1, 1, 0)]);
    let mut visited = HashSet::from([(1, 1)]);

    while let Some((x, y, steps)) = to_visit.pop_front() {
        for (nx, ny) in utils::neighbours(x, y, usize::MAX / 3, usize::MAX / 3, false) {
            if is_open_space(nx, ny, fav_number) && !visited.contains(&(nx, ny)) && steps < 50 {
                visited.insert((nx, ny));
                to_visit.push_back((nx, ny, steps + 1));
            }
        }
    }

    visited.len()
}

fn is_open_space(x: usize, y: usize, fav_number: usize) -> bool {
    count_one_bits(coordinate_value(x, y) + fav_number) % 2 == 0
}

fn count_one_bits(n: usize) -> usize {
    #[tailcall]
    fn dfs(n: usize, count: usize) -> usize {
        if n == 0 { count } else { dfs(n / 2, count + n % 2) }
    }
    dfs(n, 0)
}

fn coordinate_value(x: usize, y: usize) -> usize {
    x * x + 3 * x + 2 * x * y + y + y * y
}
