pub struct Joker {
    joker_type: JokerType,
    edition: Edition,
}

// TODO: Remove
struct JokerType;

pub struct JokerEffectType {
    chips: bool,
    add_mult: bool,
    mult_mult: bool,
    effect: bool,
    retrigger: bool,
    economy: bool,
}
impl JokerEffectType {
    pub fn new(
        chips: bool,
        add_mult: bool,
        mult_mult: bool,
        effect: bool,
        retrigger: bool,
        economy: bool,
    ) -> Self {
        Self {
            chips,
            add_mult,
            mult_mult,
            effect,
            retrigger,
            economy,
        }
    }
}

pub struct JokerCompatibility {
    pub copyable: bool,
    pub perishable: bool,
    pub eternal: bool,
}

pub enum Edition {
    Base,
    Foil,
    Holographic,
    Polychrome,
    Negative,
}

pub enum Rarity {
    Common,
    Uncommon,
    Rare,
    Legendary,
}

pub enum Consumable {
    Tarot(Tarot),
    Planet(Planet),
    Spectral(Spectral),
}

pub struct Tarot {}
pub struct Planet {}
pub struct Spectral {}

pub struct Deck {}

// CODEGEN START
// CODEGEN END
