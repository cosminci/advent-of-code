use md5::compute;

pub fn solve() {
    let input = String::from("bgvyzdsv");

    println!("Part 1: {}", lowest_number(&input, 5));
    println!("Part 2: {}", lowest_number(&input, 6));
}

fn lowest_number(key: &String, leading_zeroes: usize) -> usize {
    let pattern = "0".repeat(leading_zeroes);
    (1..).find(|n| {
        let md5 = format!("{:x}", compute(format!("{}{}", key, n)));
        md5.starts_with(&pattern)
    }).unwrap()
}
