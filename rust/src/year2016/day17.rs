use std::collections::VecDeque;

pub fn solve() {
    let input = "ioramepc";

    println!("Part 1: {}", shortest_path(input));
    println!("Part 2: {}", longest_path(input));
}

fn shortest_path(passcode: &str) -> String {
    let mut to_visit = VecDeque::from([(0, 0, String::new())]);

    while let Some((x, y, path)) = to_visit.pop_front() {
        if x == 3 && y == 3 { return String::from(path) }

        expand_search_space(passcode, &mut to_visit, x, y, path)
    }

    String::new()
}

fn longest_path(passcode: &str) -> usize {
    let mut to_visit = VecDeque::from([(0, 0, String::new())]);

    let mut path_lengths = vec![];
    while let Some((x, y, path)) = to_visit.pop_front() {
        if x == 3 && y == 3 {
            path_lengths.push(path.len())
        } else {
            expand_search_space(passcode, &mut to_visit, x, y, path)
        }
    }

    *path_lengths.iter().max().unwrap()
}

fn expand_search_space(
    passcode: &str,
    to_visit: &mut VecDeque<(usize, usize, String)>,
    x: usize, y: usize,
    path: String,
) {
    let digest = md5::compute(format!("{}{}", passcode, path));
    let states = format!("{:x}", digest).chars().take(4).collect::<String>();

    for (idx, door_state) in states.chars().enumerate() {
        if ['b', 'c', 'd', 'e', 'f'].contains(&door_state) {
            index_to_neighbour(idx, x, y)
                .iter()
                .for_each(|(nx, ny, d)|
                    to_visit.push_back((*nx, *ny, format!("{}{}", path, d)))
                )
        }
    }
}

fn index_to_neighbour(idx: usize, x: usize, y: usize) -> Option<(usize, usize, char)> {
    let (nx, ny, d) = match idx {
        0 => (x as isize - 1, y as isize, 'U'),
        1 => (x as isize + 1, y as isize, 'D'),
        2 => (x as isize, y as isize - 1, 'L'),
        _ => (x as isize, y as isize + 1, 'R')
    };
    if nx >= 0 && nx < 4 && ny >= 0 && ny < 4 {
        Some((nx as usize, ny as usize, d))
    } else { None }
}
