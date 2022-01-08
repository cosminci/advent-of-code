use serde_json::{Map, Value};

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2015/resources/day12.txt");
    let json = serde_json::from_str(&input.first().unwrap()).unwrap();
    println!("Part 1: {}", sum_nums(&json, false));
    println!("Part 2: {}", sum_nums(&json, true));
}

fn sum_nums(json: &Value, no_red: bool) -> i64 {
    match json {
        Value::Number(num) =>
            num.as_i64().unwrap(),
        Value::Object(obj) =>
            if no_red && has_red(obj) {
                0
            } else {
                obj.iter().map(|(_, v)| sum_nums(v, no_red)).sum()
            },
        Value::Array(arr) =>
            arr.iter().map(|v| sum_nums(v, no_red)).sum(),
        _ => 0
    }
}

fn has_red(obj: &Map<String, Value>) -> bool {
    obj.iter().any(|(_, v)| v.as_str().map_or(false, |s| s == "red"))
}
