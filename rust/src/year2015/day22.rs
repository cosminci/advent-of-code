use boolinator::Boolinator;
use memoize::memoize;

pub fn solve() {
    let (player_hp, mana_left) = (50, 500);
    let (boss_hp, boss_damage) = (51, 9);

    println!("Part 1: {}", minimum_mana_to_win(player_hp, boss_hp, boss_damage, mana_left, 0,
                                               [0; 3], true, false));
    println!("Part 2: {}", minimum_mana_to_win(player_hp, boss_hp, boss_damage, mana_left, 0,
                                               [0; 3], true, true));
}

#[memoize]
fn minimum_mana_to_win(player_hp: i8, boss_hp: i8, boss_damage: i8, mana_left: i16, mana_used: u64,
                       effect_ttls: [u8; 3], player_turn: bool, hard: bool) -> u64 {
    let boss_hp = if effect_ttls[1] > 0 { boss_hp - 3 } else { boss_hp };
    if boss_hp <= 0 {
        return mana_used
    }

    let mana_left = if effect_ttls[2] > 0 { mana_left + 101 } else { mana_left };
    if !player_turn {
        take_damage(player_hp, boss_hp, boss_damage, mana_left, mana_used, effect_ttls, hard)
    } else {
        cast_spell(player_hp, boss_hp, boss_damage, mana_left, mana_used, effect_ttls, hard)
    }
}

fn take_damage(player_hp: i8, boss_hp: i8, boss_damage: i8, mana_left: i16, mana_used: u64,
               effect_ttls: [u8; 3], hard: bool) -> u64 {
    let damage = boss_damage - (effect_ttls[0] > 0).as_some(7).unwrap_or(0);
    let hp_left = player_hp - damage;
    if hp_left <= 0 {
        u64::MAX
    } else {
        minimum_mana_to_win(hp_left, boss_hp, boss_damage, mana_left, mana_used,
                            effect_ttls.map(|ttl| ttl.saturating_sub(1)), true, hard)
    }
}

fn cast_spell(player_hp: i8, boss_hp: i8, boss_damage: i8, mana_left: i16, mana_used: u64,
              effect_ttls: [u8; 3], hard: bool) -> u64 {
    (hard && player_hp <= 1).as_some(u64::MAX).unwrap_or({
        let spells: Vec<Box<dyn Spell>> = vec![
            Box::new(MagicMissile),
            Box::new(Drain),
            Box::new(Shield),
            Box::new(Poison),
            Box::new(Recharge),
        ];
        spells.into_iter()
            .flat_map(|s| {
                let new_effect_ttls = effect_ttls.map(|ttl| ttl.saturating_sub(1));
                let new_player_hp = player_hp - hard.as_some(1).unwrap_or(0);
                s.cast(new_player_hp, mana_left, mana_used, new_effect_ttls, boss_hp)
                    .map(|(hp_left, mana_left, mana_used, effect_ttls, boss_hp)|
                        minimum_mana_to_win(hp_left, boss_hp, boss_damage, mana_left, mana_used,
                                            effect_ttls, false, hard)
                    )
            }).min().unwrap_or(u64::MAX)
    })
}

trait Spell {
    fn cast(&self, player_hp: i8, mana_left: i16, mana_used: u64,
            effects: [u8; 3], boss_hp: i8) -> Option<(i8, i16, u64, [u8; 3], i8)>;
}

struct MagicMissile;

impl Spell for MagicMissile {
    fn cast(&self, player_hp: i8, mana_left: i16, mana_used: u64,
            effects: [u8; 3], boss_hp: i8) -> Option<(i8, i16, u64, [u8; 3], i8)> {
        (mana_left >= 53).as_some((player_hp, mana_left - 53, mana_used + 53, effects, boss_hp - 4))
    }
}

struct Drain;

impl Spell for Drain {
    fn cast(&self, player_hp: i8, mana_left: i16, mana_used: u64,
            effects: [u8; 3], boss_hp: i8) -> Option<(i8, i16, u64, [u8; 3], i8)> {
        (mana_left >= 73)
            .as_some((player_hp + 2, mana_left - 73, mana_used + 73, effects, boss_hp - 2))
    }
}

struct Shield;

impl Spell for Shield {
    fn cast(&self, player_hp: i8, mana_left: i16, mana_used: u64,
            effects: [u8; 3], boss_hp: i8) -> Option<(i8, i16, u64, [u8; 3], i8)> {
        (effects[0] == 0 && mana_left >= 113).as_some({
            let mut new_effects = effects.clone();
            new_effects[0] = 6;
            (player_hp, mana_left - 113, mana_used + 113, new_effects, boss_hp)
        })
    }
}

struct Poison;

impl Spell for Poison {
    fn cast(&self, player_hp: i8, mana_left: i16, mana_used: u64,
            effects: [u8; 3], boss_hp: i8) -> Option<(i8, i16, u64, [u8; 3], i8)> {
        (effects[1] == 0 && mana_left >= 173).as_some({
            let mut new_effects = effects.clone();
            new_effects[1] = 6;
            (player_hp, mana_left - 173, mana_used + 173, new_effects, boss_hp)
        })
    }
}

struct Recharge;

impl Spell for Recharge {
    fn cast(&self, player_hp: i8, mana_left: i16, mana_used: u64,
            effects: [u8; 3], boss_hp: i8) -> Option<(i8, i16, u64, [u8; 3], i8)> {
        (effects[2] == 0 && mana_left >= 229).as_some({
            let mut new_effects = effects.clone();
            new_effects[2] = 5;
            (player_hp, mana_left - 229, mana_used + 229, new_effects, boss_hp)
        })
    }
}
