use itertools::Itertools;
use regex::Regex;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2016/resources/day3.txt");
    let triangles = parse_input(input);

    println!("Part 1: {}", count_valid_triangles(&triangles));
    println!("Part 2: {}", count_valid_triangles_vertically(&triangles));
}

type Triangle = (u16, u16, u16);

fn count_valid_triangles(triangles: &Vec<Triangle>) -> usize {
    triangles.iter()
        .filter(|(a, b, c)| is_valid_triangle(*a, *b, *c))
        .count()
}

fn count_valid_triangles_vertically(triangles: &Vec<Triangle>) -> usize {
    [nth_edge(triangles, |t| t.0), nth_edge(triangles, |t| t.1), nth_edge(triangles, |t| t.2)]
        .concat()
        .chunks(3)
        .filter(|t| is_valid_triangle(t[0], t[1], t[2]))
        .count()
}

fn nth_edge<F: Fn(Triangle) -> u16>(triangles: &Vec<Triangle>, nth: F) -> Vec<u16> {
    triangles.iter()
        .map(|(a, b, c)| nth((*a, *b, *c)))
        .collect_vec()
}

fn is_valid_triangle(a: u16, b: u16, c: u16) -> bool {
    a + b > c && b + c > a && a + c > b
}

fn parse_input(input: Vec<String>) -> Vec<Triangle> {
    let spaces = Regex::new(r" {1,4}").unwrap();
    input.iter().map(|s| {
        let edges = spaces
            .replace_all(s.trim(), " ")
            .split(' ')
            .map(|edge| edge.trim().parse().unwrap())
            .collect_vec();
        (edges[0], edges[1], edges[2])
    }).collect_vec()
}
