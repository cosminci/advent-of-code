use std::collections::{HashSet as MutHashSet, VecDeque};

use im_rc::{hashset, HashSet, Vector, vector};
use itertools::Itertools;

pub fn solve() {
    println!("Part 1: {}", min_steps_to_fill_last_floor(initial_state_1()));
    println!("Part 2: {}", min_steps_to_fill_last_floor(initial_state_2()));
}

fn min_steps_to_fill_last_floor(state: State) -> usize {
    let total_items = count_items(&state.0);

    let mut to_visit = VecDeque::from([(state.clone(), 0)]);
    let mut visited = MutHashSet::from([normalize(&state)]);

    while let Some((curr_state, steps)) = to_visit.pop_front() {
        let floors = &curr_state.0;
        let (rtgs, chips) = floors.last().unwrap();

        if rtgs.len() + chips.len() == total_items {
            return steps;
        } else {
            for next_state in possible_next_states(curr_state) {
                if visited.insert(normalize(&next_state)) {
                    to_visit.push_back((next_state, steps + 1));
                }
            }
        }
    }
    usize::MAX
}

fn normalize(state: &State) -> (Vec<(usize, usize, usize)>, usize) {
    let (floors, curr_floor) = state;

    let new_floors = floors.iter().map(|(rtgs, chips)| {
        (rtgs.clone().intersection(chips.clone()).len(),
         rtgs.clone().relative_complement(chips.clone()).len(),
         chips.clone().relative_complement(rtgs.clone()).len())
    }).collect_vec();

    (new_floors, *curr_floor)
}

fn count_items(floors: &Vector<Floor>) -> usize {
    floors.iter().map(|(rtgs, chips)| rtgs.len() + chips.len()).sum()
}

fn possible_next_states(state: State) -> Vec<State> {
    let (floors, curr_floor) = state;
    let floor @ (rtgs, chips) = &floors[curr_floor];
    let combinations = item_combinations(floor);

    combinations.iter().flat_map(|(rtgs_to_move, chips_to_move)| {
        let remaining_rtgs = rtgs.clone().relative_complement(rtgs_to_move.clone());
        let remaining_chips = chips.clone().relative_complement(chips_to_move.clone());

        if !is_valid_floor(&remaining_rtgs, &remaining_chips) {
            vec![]
        } else {
            next_floors(curr_floor).iter().filter_map(|&next_floor| {
                let (rtgs, chips) = &floors[next_floor];
                let next_floor_rtgs = rtgs.clone().union(rtgs_to_move.clone());
                let next_floor_chips = chips.clone().union(chips_to_move.clone());

                if !is_valid_floor(&next_floor_rtgs, &next_floor_chips) {
                    None
                } else {
                    let new_floors = floors
                        .update(curr_floor, (remaining_rtgs.clone(), remaining_chips.clone()))
                        .update(next_floor, (next_floor_rtgs, next_floor_chips));
                    Some((new_floors, next_floor))
                }
            }).collect_vec()
        }
    }).collect_vec()
}

fn next_floors(curr_floor: usize) -> Vec<usize> {
    match curr_floor {
        0 => vec![1],
        1 => vec![0, 2],
        2 => vec![1, 3],
        _ => vec![2]
    }
}

fn item_combinations(floor: &Floor) -> Vec<Floor> {
    let (rtgs, chips) = floor;

    let rtgs_to_take = single_type_combinations(rtgs).into_iter()
        .map(|rtg_combo| (rtg_combo, hashset![])).collect_vec();

    let chips_to_take = single_type_combinations(chips).into_iter()
        .map(|chip_combo| (hashset![], chip_combo)).collect_vec();

    let mixed_to_take = rtgs.iter()
        .flat_map(|&rtg| chips.iter().map(move |&chip| (hashset![rtg], hashset![chip])))
        .collect_vec();

    [rtgs_to_take, chips_to_take, mixed_to_take].concat()
}

fn single_type_combinations(items: &HashSet<u8>) -> Vec<HashSet<u8>> {
    let pairs = items.iter()
        .combinations(2)
        .map(|c| c.into_iter().map(|&v| v).collect())
        .collect_vec();

    let single = items.iter().map(|&id| hashset![id]).collect_vec();

    [pairs, single].concat()
}

fn is_valid_floor(rtgs: &HashSet<u8>, chips: &HashSet<u8>) -> bool {
    rtgs.len() == 0 || chips.iter().all(|chip| rtgs.contains(chip))
}

fn initial_state_1() -> State {
    let floor0 = (hashset![0], hashset![0]);
    let floor1 = (hashset![1, 2, 3, 4], hashset![]);
    let floor2 = (hashset![], hashset![1, 2, 3, 4]);
    let floor3 = (hashset![], hashset![]);
    let initial_floor = 0;

    (vector![floor0, floor1, floor2, floor3], initial_floor)
}

fn initial_state_2() -> State {
    let (floors, initial_floor) = initial_state_1();
    let (first_floor_rtgs, first_floor_chips) = &floors[0];

    let new_floors = floors.update(0, (
        first_floor_rtgs.clone().union(hashset![5, 6]),
        first_floor_chips.clone().union(hashset![5, 6])),
    );

    (new_floors, initial_floor)
}

type Floor = (HashSet<u8>, HashSet<u8>);
type State = (Vector<Floor>, usize);
