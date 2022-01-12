use std::{fs::File, io::{BufReader, prelude::*}, path::Path};
use std::fmt::Debug;
use std::str::FromStr;

use itertools::Itertools;

pub fn read_lines(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.unwrap())
        .collect()
}

pub fn read_lines_as<T>(filename: impl AsRef<Path>) -> Vec<T>
    where T: FromStr, <T as FromStr>::Err: Debug, {
    read_lines(filename).iter().map(|line| line.parse::<T>().unwrap()).collect_vec()
}

pub fn read_first_line(filename: impl AsRef<Path>) -> String {
    read_lines(filename).first().unwrap().to_string()
}

pub fn neighbours(row: usize, col: usize, rows: usize, cols: usize, diag: bool) -> Vec<(usize, usize)> {
    let (r, c) = (row as isize, col as isize);
    let (m, n) = (rows as isize, cols as isize);
    let straight = vec![(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)];
    let diags = vec![(r - 1, c - 1), (r - 1, c + 1), (r + 1, c - 1), (r + 1, c + 1)];

    let to_check = if !diag { straight } else { [straight, diags].concat() };
    to_check.into_iter()
        .filter(|&(r, c)| r >= 0 && c >= 0 && r < m && c < n)
        .map(|(r, c)| (r as usize, c as usize))
        .collect_vec()
}
