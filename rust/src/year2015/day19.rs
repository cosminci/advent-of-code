use std::borrow::Borrow;
use std::collections::{HashMap, HashSet, VecDeque};

use itertools::Itertools;
use substring::Substring;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2015/resources/day19.txt");

    let (molecule, replacements) = parse_input(input);
    println!("Part 1: {:?}", generate(&molecule, &replacements).len());

    let reverse_replacements = group(replacements.iter()
        .flat_map(|(k, v)|
            v.iter().map(|v| (v.clone(), k.clone()))
        ).collect());
    println!("Part 2: {:?}", steps_to_medicine(&molecule, &reverse_replacements));
}

fn steps_to_medicine(medicine: &String, replacements: &HashMap<String, Vec<String>>) -> usize {
    let mut to_visit = VecDeque::from([(medicine.clone(), 0)]);
    let mut visited: HashSet<String> = HashSet::from_iter([medicine.clone()]);
    loop {
        let (curr, steps) = to_visit.pop_front().unwrap();
        if curr.as_str() == "e" {
            return steps
        }
        let new_molecules = generate(&curr, &replacements);
        let new_molecule = new_molecules.iter().min_by_key(|m| m.len()).unwrap();
        if !visited.contains(new_molecule) {
            to_visit.push_back((new_molecule.clone(), steps + 1));
            visited.insert(new_molecule.clone());
        }
    }
}

fn generate(molecule: &String, replacements: &HashMap<String, Vec<String>>) -> Vec<String> {
    replacements.iter()
        .fold(vec![], |acc, (source, targets)| {
            let new_molecules = (0..molecule.len() - 1).rev().flat_map(|idx| {
                if molecule.substring(idx, idx + source.len()) != source.as_str() {
                    vec![]
                } else {
                    targets.iter().map(|target| {
                        let mut new_molecule = molecule.clone();
                        new_molecule.replace_range(idx..idx + source.len(), target);
                        new_molecule
                    }).collect_vec()
                }
            }).collect_vec();
            [acc, new_molecules].concat()
        }).into_iter().unique().collect_vec()
}

fn parse_input(input: Vec<String>) -> (String, HashMap<String, Vec<String>>) {
    let molecule = input.last().unwrap().to_owned();

    let replacements = group(input.iter()
        .take_while(|line| !line.is_empty())
        .map(|line| {
            let parts = line.split(" => ").collect_vec();
            (parts[0].to_string(), parts[1].to_string())
        }).collect());

    (molecule, replacements)
}

fn group(vec: Vec<(String, String)>) -> HashMap<String, Vec<String>> {
    vec.iter()
        .into_group_map_by(|(k, _)| k.to_owned())
        .borrow().into_iter()
        .map(|(k, v)| {
            (k.to_string(), v.iter().map(|(_, v)| v.to_string()).collect_vec())
        })
        .collect()
}
