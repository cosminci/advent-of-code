use frunk::monoid::Monoid;
use frunk::semigroup::Semigroup;
use itertools::{iproduct, Itertools};

pub fn solve() {
    let boss_stats = Stats { damage: 8, armor: 2 };
    let (player_hp, boss_hp) = (100, 108);

    let mut loadouts = loadouts();

    println!("Part 1: {}", cheapest_loadout_that_wins(&mut loadouts, player_hp, &boss_stats, boss_hp));
    println!("Part 2: {}", most_expensive_loadout_that_loses(&mut loadouts, player_hp, &boss_stats, boss_hp));
}

fn cheapest_loadout_that_wins(loadouts: &mut Vec<Item>, player_hp: i8, boss_stats: &Stats, boss_hp: i8) -> u16 {
    loadouts.sort_by_key(|s| s.cost);

    loadouts.iter().find(|loadout| {
        will_win(&loadout.stats, player_hp, boss_stats, boss_hp)
    }).map(|loadout| loadout.cost).unwrap()
}

fn most_expensive_loadout_that_loses(loadouts: &mut Vec<Item>, player_hp: i8, boss_stats: &Stats, boss_hp: i8) -> u16 {
    loadouts.reverse();

    loadouts.iter().find(|loadout| {
        !will_win(&loadout.stats, player_hp, boss_stats, boss_hp)
    }).map(|loadout| loadout.cost).unwrap()
}

fn will_win(stats1: &Stats, hp1: i8, stats2: &Stats, hp2: i8) -> bool {
    if hp1 < 0 {
        false
    } else if hp2 < 0 {
        true
    } else {
        let new_hp2 = hp2 - stats1.damage.saturating_sub(stats2.armor).max(1) as i8;
        !will_win(stats2, new_hp2, stats1, hp1)
    }
}

fn loadouts() -> Vec<Item> {
    let weapons = vec![
        Item { stats: Stats { damage: 4, armor: 0 }, cost: 8 },
        Item { stats: Stats { damage: 5, armor: 0 }, cost: 10 },
        Item { stats: Stats { damage: 6, armor: 0 }, cost: 25 },
        Item { stats: Stats { damage: 7, armor: 0 }, cost: 40 },
        Item { stats: Stats { damage: 8, armor: 0 }, cost: 74 },
    ];
    let armors = vec![
        Item { stats: Stats { damage: 0, armor: 1 }, cost: 13 },
        Item { stats: Stats { damage: 0, armor: 2 }, cost: 31 },
        Item { stats: Stats { damage: 0, armor: 3 }, cost: 53 },
        Item { stats: Stats { damage: 0, armor: 4 }, cost: 75 },
        Item { stats: Stats { damage: 0, armor: 5 }, cost: 102 },
    ];
    let rings = vec![
        Item { stats: Stats { damage: 1, armor: 0 }, cost: 25 },
        Item { stats: Stats { damage: 2, armor: 0 }, cost: 50 },
        Item { stats: Stats { damage: 3, armor: 0 }, cost: 100 },
        Item { stats: Stats { damage: 0, armor: 1 }, cost: 20 },
        Item { stats: Stats { damage: 0, armor: 2 }, cost: 40 },
        Item { stats: Stats { damage: 0, armor: 3 }, cost: 80 },
    ];
    let two_rings = rings.iter()
        .combinations(2)
        .map(|c| c[0].combine(&c[1]))
        .collect_vec();
    let ring_loadouts = [two_rings, rings, vec![Item::empty()]].concat();
    let armor_loadouts = [armors, vec![Item::empty()]].concat();

    iproduct!(&weapons, &armor_loadouts, &ring_loadouts).map(|(w, a, r)| {
        w.combine(a).combine(r)
    }).collect_vec()
}

#[derive(Copy, Clone)]
struct Stats {
    damage: u8,
    armor: u8,
}

impl Semigroup for Stats {
    fn combine(&self, other: &Self) -> Self {
        Stats {
            damage: self.damage + other.damage,
            armor: self.armor + other.armor,
        }
    }
}

#[derive(Copy, Clone)]
struct Item {
    stats: Stats,
    cost: u16,
}

impl Semigroup for Item {
    fn combine(&self, other: &Self) -> Self {
        Item {
            stats: self.stats.combine(&other.stats),
            cost: self.cost + other.cost,
        }
    }
}

impl Monoid for Item {
    fn empty() -> Self {
        Item { stats: Stats { damage: 0, armor: 0 }, cost: 0 }
    }
}
