use itertools::{iproduct, Itertools};

pub fn solve() {
    let boss_gear = Gear { damage: 8, armor: 2, cost: 0 };
    let (player_hp, boss_hp) = (100, 108);

    let mut loadouts = loadouts();

    println!("Part 1: {}", cheapest_loadout_that_wins(&mut loadouts, player_hp, &boss_gear, boss_hp));
    println!("Part 2: {}", most_expensive_loadout_that_loses(&mut loadouts, player_hp, &boss_gear, boss_hp));
}

fn cheapest_loadout_that_wins(loadouts: &mut Vec<Gear>, player_hp: i8, boss_stats: &Gear, boss_hp: i8) -> u16 {
    loadouts.sort_by_key(|s| s.cost);

    loadouts.iter().find(|loadout| {
        will_win(loadout, player_hp, boss_stats, boss_hp)
    }).map(|loadout| loadout.cost).unwrap()
}

fn most_expensive_loadout_that_loses(loadouts: &mut Vec<Gear>, player_hp: i8, boss_stats: &Gear, boss_hp: i8) -> u16 {
    loadouts.reverse();

    loadouts.iter().find(|loadout| {
        !will_win(loadout, player_hp, boss_stats, boss_hp)
    }).map(|loadout| loadout.cost).unwrap()
}

fn will_win(stats1: &Gear, hp1: i8, stats2: &Gear, hp2: i8) -> bool {
    if hp1 < 0 {
        false
    } else if hp2 < 0 {
        true
    } else {
        let new_hp2 = hp2 - (stats1.damage - stats2.armor).max(1);
        !will_win(stats2, new_hp2, stats1, hp1)
    }
}

fn loadouts() -> Vec<Gear> {
    let weapons = vec![
        Gear { damage: 4, armor: 0, cost: 8 },
        Gear { damage: 5, armor: 0, cost: 10 },
        Gear { damage: 6, armor: 0, cost: 25 },
        Gear { damage: 7, armor: 0, cost: 40 },
        Gear { damage: 8, armor: 0, cost: 74 },
    ];
    let armors = vec![
        Gear { damage: 0, armor: 1, cost: 13 },
        Gear { damage: 0, armor: 2, cost: 31 },
        Gear { damage: 0, armor: 3, cost: 53 },
        Gear { damage: 0, armor: 4, cost: 75 },
        Gear { damage: 0, armor: 5, cost: 102 },
    ];
    let rings = vec![
        Gear { damage: 1, armor: 0, cost: 25 },
        Gear { damage: 2, armor: 0, cost: 50 },
        Gear { damage: 3, armor: 0, cost: 100 },
        Gear { damage: 0, armor: 1, cost: 20 },
        Gear { damage: 0, armor: 2, cost: 40 },
        Gear { damage: 0, armor: 3, cost: 80 },
    ];
    let two_rings = rings.iter()
        .combinations(2)
        .map(|c| { c[0].combine(&c[1]) })
        .collect_vec();
    let ring_loadouts = [two_rings, rings, vec![Gear::empty()]].concat();
    let armor_loadouts = [armors, vec![Gear::empty()]].concat();

    iproduct!(&weapons, &armor_loadouts, &ring_loadouts).map(|(w, a, r)| {
        w.combine(a).combine(r)
    }).collect_vec()
}

#[derive(Copy, Clone)]
struct Gear {
    damage: i8,
    armor: i8,
    cost: u16,
}

impl Gear {
    fn empty() -> Self { Self { damage: 0, armor: 0, cost: 0 } }
    fn combine(self, other: &Self) -> Self {
        Self {
            damage: self.damage + other.damage,
            armor: self.armor + other.armor,
            cost: self.cost + other.cost,
        }
    }
}
