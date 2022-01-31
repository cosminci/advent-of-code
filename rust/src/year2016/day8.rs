use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day8.txt");
    let commands = parse_input(input);

    let mut screen = [[false; 50]; 6];
    apply_commands(&commands, &mut screen);

    println!("Part 1: {}", count_pixels(&screen));
    println!("Part 2:");
    print_screen(&screen);
}

enum Command {
    Rect(usize, usize),
    RotateCol(usize, usize),
    RotateRow(usize, usize),
}

fn count_pixels(screen: &[[bool; 50]; 6]) -> usize {
    screen
        .map(|row| row.into_iter()
            .filter(|&pixel| pixel)
            .count())
        .iter().sum()
}

fn print_screen(screen: &[[bool; 50]; 6]) {
    for x in 0..screen.len() {
        println!("{}", screen[x].map(|pixel| if pixel { "**" } else { "  " }).concat());
    }
}

fn apply_commands(commands: &Vec<Command>, screen: &mut [[bool; 50]; 6]) {
    for cmd in commands {
        match cmd {
            &Command::Rect(yr, xr) =>
                for x in 0..xr {
                    for y in 0..yr {
                        screen[x][y] = true
                    }
                },

            &Command::RotateRow(idx, amount) =>
                screen[idx].rotate_right(amount),

            &Command::RotateCol(idx, amount) => {
                let mut col = screen.map(|row| row[idx]).into_iter().collect_vec();
                col.rotate_right(amount);
                for x in 0..6 {
                    screen[x][idx] = col[x];
                }
            }
        }
    }
}

fn parse_input(input: Vec<String>) -> Vec<Command> {
    input.iter().map(|line| {
        let parts = line.split(' ').collect_vec();

        if parts[0] == "rect" {
            let xy = parts[1].split('x').collect_vec();

            Command::Rect(xy[0].parse().unwrap(), xy[1].parse().unwrap())
        } else {
            let idx = parts[2].split('=').collect_vec()[1].parse().unwrap();
            let dir = parts[4].parse().unwrap();

            if parts[1] == "column" {
                Command::RotateCol(idx, dir)
            } else {
                Command::RotateRow(idx, dir)
            }
        }
    }).collect_vec()
}
