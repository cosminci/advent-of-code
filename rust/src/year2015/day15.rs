use itertools::Itertools;

use crate::utils;

pub fn solve() {
    let input = utils::read_lines("./src/year2015/resources/day15.txt");
    let ingredients = parse_input(input);

    println!("Part 1: {}", highest_score(&ingredients, 100));
    println!("Part 2: {}", highest_score_500_calories(&ingredients, 100));
}

fn highest_score(ingredients: &Vec<Ingredient>, teaspoons: u8) -> isize {
    scores(&ingredients, teaspoons).into_iter().map(|(score, _)| score).max().unwrap()
}

fn highest_score_500_calories(ingredients: &Vec<Ingredient>, teaspoons: u8) -> isize {
    scores(&ingredients, teaspoons).into_iter()
        .filter(|&(_, calories)| calories == 500)
        .map(|(score, _)| score)
        .max().unwrap()
}

fn scores(ingredients: &Vec<Ingredient>, teaspoons: u8) -> Vec<(isize, u16)> {
    recipes(ingredients.len() as u8, teaspoons).iter()
        .map(|recipe| {
            let (capacity, durability, flavor, texture, calories) = recipe.iter().enumerate()
                .fold((0, 0, 0, 0, 0),
                      |(capacity, durability, flavor, texture, calories), (i, &count)| (
                          capacity + ingredients[i].capacity * count as isize,
                          durability + ingredients[i].durability * count as isize,
                          flavor + ingredients[i].flavor * count as isize,
                          texture + ingredients[i].texture * count as isize,
                          calories + ingredients[i].calories * count as u16
                      ),
                );
            (vec![capacity, durability, flavor, texture].iter()
                 .map(|&value| if value < 0 { 0 } else { value })
                 .product(), calories)
        }).collect_vec()
}

fn recipes(ingredients: u8, teaspoons: u8) -> Vec<Vec<u8>> {
    if ingredients == 1 {
        vec![vec![teaspoons]]
    } else {
        (0..=teaspoons).flat_map(|teaspoons_to_use|
            recipes(ingredients - 1, teaspoons - teaspoons_to_use).iter().map(|c| {
                let mut combination = c.clone();
                combination.push(teaspoons_to_use);
                combination
            }).collect_vec()
        ).collect_vec()
    }
}

struct Ingredient {
    capacity: isize,
    durability: isize,
    flavor: isize,
    texture: isize,
    calories: u16,
}

fn parse_input(input: Vec<String>) -> Vec<Ingredient> {
    input.iter().map(|line| {
        let parts = line.split(" ").collect_vec();
        Ingredient {
            capacity: parts[2].parse().unwrap(),
            durability: parts[4].parse().unwrap(),
            flavor: parts[6].parse().unwrap(),
            texture: parts[8].parse().unwrap(),
            calories: parts[10].parse().unwrap(),
        }
    }).collect_vec()
}
