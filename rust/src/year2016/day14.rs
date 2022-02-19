use std::collections::HashMap;

use itertools::Itertools;

pub fn solve() {
    let salt = "ihaygndm";

    println!("Part 1: {}", index_of_nth_key(64, salt, &generate_hash));
    println!("Part 2: {}", index_of_nth_key(64, salt, &generate_stretched_hash));
}

fn index_of_nth_key<F: Fn(&str, usize) -> String>(nth: u8, salt: &str, hash_gen_fn: &F) -> usize {
    let mut hashes = HashMap::new();
    let mut count = 0;
    let mut idx = 0;
    loop {
        if count == nth { return idx - 1 }
        match has_triplet(lookup_or_generate(&mut hashes, salt, idx, hash_gen_fn)) {
            None => (),
            Some(char) =>
                if (1..=1000).any(|i| {
                    let hash = lookup_or_generate(&mut hashes, salt, idx + i, hash_gen_fn);
                    has_quintet(hash, char)
                }) {
                    count += 1;
                }
        }
        idx += 1;
    }
}

fn lookup_or_generate<'a, F: Fn(&str, usize) -> String>(
    hashes: &'a mut HashMap<usize, String>,
    salt: &str,
    idx: usize,
    hash_gen_fn: F,
) -> &'a String {
    if !hashes.contains_key(&idx) {
        let hash = hash_gen_fn(salt, idx);
        hashes.insert(idx, hash);
    }
    &hashes[&idx]
}

fn generate_hash(salt: &str, idx: usize) -> String {
    format!("{:x}", md5::compute(format!("{}{}", salt, idx)))
}

fn generate_stretched_hash(salt: &str, idx: usize) -> String {
    (1..=2016).fold(generate_hash(salt, idx), |prev, _| format!("{:x}", md5::compute(prev)))
}

fn has_triplet(hash: &String) -> Option<char> {
    hash.chars().tuple_windows().find_map(|(a, b, c)| {
        if a == b && b == c { Some(a) } else { None }
    })
}

fn has_quintet(hash: &String, char: char) -> bool {
    hash.chars().tuple_windows().any(|(a, b, c, d, e)| {
        a == char && a == b && a == c && a == d && a == e
    })
}
