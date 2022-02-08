use itertools::Itertools;

pub fn solve() {
    let input = "10111011111001111";

    println!("Part 1: {}", compute_checksum(input, 272));
    println!("Part 2: {}", compute_checksum(input, 35651584));
}

fn compute_checksum(seed: &str, disk_size: usize) -> String {
    checksum(fill(seed, disk_size))
}

fn checksum(s: String) -> String {
    let result = s.chars()
        .tuples()
        .map(|(a, b)| if a == b { '1' } else { '0' })
        .collect::<String>();
    if result.len() % 2 == 1 { result } else { checksum(result) }
}

fn fill(input: &str, disk_size: usize) -> String {
    let mut data = String::from(input);
    while data.len() < disk_size {
        let copy = data
            .clone()
            .chars()
            .rev()
            .map(|d| if d == '1' { '0' } else { '1' })
            .collect::<String>();
        data.push('0');
        data.push_str(copy.as_str());
    }
    data.chars().take(disk_size).collect()
}
