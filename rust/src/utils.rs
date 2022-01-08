use std::{
    fs::File,
    io::{BufReader, prelude::*},
    path::Path,
};

pub fn read_lines(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.unwrap())
        .collect()
}

pub fn read_first_line(filename: impl AsRef<Path>) -> String {
    read_lines(filename).first().unwrap().to_string()
}
