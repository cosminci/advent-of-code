use crate::utils;

pub fn solve() {
    let grid = utils::read_lines("./src/year2015/resources/day18.txt");
    let (m, n) = (grid.len(), grid.first().unwrap().len());

    println!("Part 1: {:?}", count_lights(animate(grid.clone(), 100, m, n, toggle_all)));
    // corners already lit on my input
    println!("Part 2: {:?}", count_lights(animate(grid, 100, m, n, toggle_no_corners)));
}

fn count_lights(grid: Vec<String>) -> usize {
    grid.iter().map(|line| line.chars().filter(|&c| c == '#').count()).sum()
}

fn animate(
    grid: Vec<String>,
    steps: u8,
    m: usize,
    n: usize,
    toggle_fn: fn(usize, char, usize, usize, usize, usize) -> char,
) -> Vec<String> {
    (0..steps).fold(grid, |grid, _| {
        (0..m).map(|r| {
            (0..n).map(|c| {
                let lit_neighbours = utils::neighbours(r, c, m, n, true).into_iter()
                    .filter(|&(nr, nc)|
                        grid[nr].chars().nth(nc).unwrap() == '#'
                    ).count();
                let current_state = grid[r].chars().nth(c).unwrap();

                toggle_fn(lit_neighbours, current_state, r, c, m, n)
            }).collect()
        }).collect()
    })
}

fn toggle_all(lit_neighbours: usize, current_state: char, _: usize, _: usize, _: usize, _: usize) -> char {
    match (current_state, lit_neighbours) {
        ('#', 2..=3) => '#',
        ('#', _) => '.',
        ('.', 3) => '#',
        _ => '.'
    }
}

fn toggle_no_corners(lit_neighbours: usize, current_state: char, r: usize, c: usize, m: usize, n: usize) -> char {
    if [(0, 0), (0, n - 1), (m - 1, 0), (m - 1, n - 1)].iter().any(|&corner| corner == (r, c)) {
        '#'
    } else {
        match (current_state, lit_neighbours) {
            ('#', 2..=3) => '#',
            ('#', _) => '.',
            ('.', 3) => '#',
            _ => '.'
        }
    }
}
