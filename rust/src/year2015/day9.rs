use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2015/resources/day9.txt");
    let routes = parse_input(input);
    let adj_list = build_adjacency_list(routes);

    println!("Part 1: {}", adj_list.keys().into_iter().flat_map(|source|
        dfs(source, &adj_list, &mut HashSet::from([source.clone()]), true)).min().unwrap()
    );
    println!("Part 2: {}", adj_list.keys().into_iter().flat_map(|source|
        dfs(source, &adj_list, &mut HashSet::from([source.clone()]), false)).max().unwrap()
    );
}

fn dfs(source: &String,
       adj_list: &HashMap<String, Vec<(String, usize)>>,
       visited: &mut HashSet<String>,
       min: bool) -> Option<usize> {
    if visited.len() == adj_list.len() { Some(0) } else {
        let path_costs = adj_list
            .get(source).unwrap().iter()
            .flat_map(|(dest, distance)| {
                if visited.insert(dest.clone()) {
                    let maybe_distance = dfs(&dest, adj_list, visited, min);
                    visited.remove(dest);
                    maybe_distance.map(|d| d + distance)
                } else { None }
            });
        if min { path_costs.min() } else { path_costs.max() }
    }
}

fn build_adjacency_list(routes: Vec<(String, String, usize)>) -> HashMap<String, Vec<(String, usize)>> {
    let mut adj_list: HashMap<String, Vec<(String, usize)>> = HashMap::new();
    for (location1, location2, distance) in routes {
        adj_list.entry(location1.clone()).or_default().push((location2.clone(), distance));
        adj_list.entry(location2).or_default().push((location1, distance));
    }
    adj_list
}

fn parse_input(input: Vec<String>) -> Vec<(String, String, usize)> {
    input.iter()
        .map(|line| {
            let parts = line.split(" = ").collect_vec();
            let locations = parts[0].split(" to ").collect_vec();
            (locations[0].to_string(), locations[1].to_string(), parts[1].parse().unwrap())
        }).collect_vec()
}
