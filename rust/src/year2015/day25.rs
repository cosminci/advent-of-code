use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;

pub fn solve() {
    println!("Part 1: {}", value_at(20151125, 2981, 3075));
}

fn value_at(first_num: u64, r: u64, c: u64) -> u64 {
    (1..).fold_while((first_num, 1, 1), |(prev_num, prev_row, prev_col), _| {
        if prev_row == r && prev_col == c {
            Done((prev_num, prev_row, prev_col))
        } else {
            let num = (prev_num * 252533) % 33554393;
            if prev_row == 1 {
                Continue((num, prev_col + 1, 1))
            } else {
                Continue((num, prev_row - 1, prev_col + 1))
            }
        }
    }).into_inner().0
}
