use itertools::Itertools;

pub fn solve() {
    let min_presents = 33100000;
    println!("Part 1: {}", first_house(min_presents, u64::MAX, 10));
    println!("Part 2: {}", first_house(min_presents, 50, 11));
}

fn first_house(min_presents: u64, max_houses: u64, presents_per_house: u64) -> u64 {
    (1..).find(|&house| {
        let x = factorize(house).into_iter()
            .filter(|f| house / f <= max_houses)
            .sum::<u64>() * presents_per_house;
        x >= min_presents
    }).unwrap()
}

fn factorize(n: u64) -> Vec<u64> {
    (1..=(n as f64).sqrt() as u64)
        .filter(|k| n % k == 0)
        .flat_map(|k| if n / k != k { vec![k, n / k] } else { vec![k] })
        .collect_vec()
}
