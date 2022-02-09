use std::collections::VecDeque;

pub fn solve() {
    let elf_count = 3004953;

    println!("Part 1: {}", surviving_elf_1(elf_count));
    println!("Part 2: {}", surviving_elf_2(elf_count));
}

fn surviving_elf_1(n: usize) -> usize {
    let mut elves: VecDeque<usize> = (1..=n).collect();
    while elves.len() > 1 {
        let thief = elves.pop_front().unwrap();
        elves.push_back(thief);
        elves.pop_front();
    }
    elves[0]
}

fn surviving_elf_2(n: usize) -> usize {
    let mut first_half: VecDeque<usize> = (1..=(n + 1) / 2).collect();
    let mut second_half: VecDeque<usize> = ((n + 1) / 2 + 1..=n).collect();

    while !first_half.is_empty() && !second_half.is_empty() {
        if first_half.len() > second_half.len() {
            first_half.pop_back();
        } else {
            second_half.pop_back();
        }
        second_half.push_front(first_half.pop_front().unwrap());
        first_half.push_back(second_half.pop_back().unwrap());
    }

    if first_half.is_empty() { second_half[0] } else { first_half[0] }
}
