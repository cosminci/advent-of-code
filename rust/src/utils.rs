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
