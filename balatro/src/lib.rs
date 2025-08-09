pub struct Joker {
    joker_type: JokerType,
    edition: Edition,
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum JokerType {
    // Common
    Joker,
    GreedyJoker,
    LustyJoker,
    WrathfulJoker,
    GluttonousJoker,
    JollyJoker,
    ZanyJoker,
    MadJoker,
    CrazyJoker,
    DrollJoker,
    SlyJoker,
    WilyJoker,
    CleverJoker,
    DeviousJoker,
    CraftyJoker,
    HalfJoker,
    CreditCard,
    Banner,
    MysticSummit,
    EightBall,
    Misprint,
    RaisedFist,
    ChaosTheClown,
    ScaryFace,
    AbstractJoker,
    DelayedGratification,
    GrosMichel,
    EvenSteven,
    OddTodd,
    Scholar,
    BusinessCard,
    Supernova,
    RideTheBus,
    Egg,
    Runner,
    IceCream,
    Splash,
    BlueJoker,
    FacelessJoker,
    GreenJoker,
    Superposition,
    ToDoList,
    Cavendish,
    RedCard,
    SquareJoker,
    RiffRaff,
    Photograph,
    ReservedParking,
    MailInRebate,
    Hallucination,
    FortuneTeller,
    Juggler,
    Drunkard,
    GoldenJoker,
    Popcorn,
    WalkieTalkie,
    SmileyFace,
    GoldenTicket,
    Swashbuckler,
    HangingChad,
    ShootTheMoon,
    // Uncommon
    JokerStencil,
    FourFingers,
    Mime,
    CeremonialDagger,
    MarbleJoker,
    LoyaltyCard,
    Dusk,
    Fibonacci,
    SteelJoker,
    Hack,
    Pareidolia,
    SpaceJoker,
    Burglar,
    Blackboard,
    SixthSense,
    Constellation,
    Hiker,
    CardSharp,
    Madness,
    Seance,
    Vampire,
    Shortcut,
    Hologram,
    CloudNine,
    Rocket,
    MidasMask,
    Luchador,
    GiftCard,
    TurtleBean,
    Erosion,
    ToTheMoon,
    StoneJoker,
    LuckyCat,
    Bull,
    DietCola,
    TradingCard,
    FlashCard,
    SpareTrousers,
    Ramen,
    Seltzer,
    Castle,
    MrBones,
    Acrobat,
    SockAndBuskin,
    Troubadour,
    Certificate,
    SmearedJoker,
    Throwback,
    RoughGem,
    Bloodstone,
    Arrowhead,
    OnyxAgate,
    GlassJoker,
    Showman,
    FlowerPot,
    MerryAndy,
    OopsAllSixes,
    TheIdol,
    SeeingDouble,
    Matador,
    Satellite,
    Cartomancer,
    Astronomer,
    Bootstraps,
    // Rare
    Dna,
    Vagabond,
    Baron,
    Obelisk,
    BaseballCard,
    AncientJoker,
    Campfire,
    Blueprint,
    WeeJoker,
    HitTheRoad,
    TheDuo,
    TheTrio,
    TheFamily,
    TheOrder,
    TheTribe,
    Stuntman,
    InvisibleJoker,
    Brainstorm,
    DriversLicense,
    BurntJoker,
    // Legendary
    Canio,
    Triboulet,
    Yorick,
    Chicot,
    Perkeo,
}
impl JokerType {
    pub fn rarity(&self) -> Rarity {
        const FIRST_UNCOMMON: u8 = JokerType::JokerStencil as u8;
        const FIRST_RARE: u8 = JokerType::Dna as u8;
        const FIRST_LEGENDARY: u8 = JokerType::Canio as u8;

        match *self as u8 {
            0..FIRST_UNCOMMON => Rarity::Common,
            FIRST_UNCOMMON..FIRST_RARE => Rarity::Uncommon,
            FIRST_RARE..FIRST_LEGENDARY => Rarity::Rare,
            _ => Rarity::Legendary,
        }
    }
    pub fn effect_type(&self) -> JokerEffectType {
        match *self {
            JokerType::Joker => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::GreedyJoker => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::LustyJoker => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::WrathfulJoker => {
                JokerEffectType::new(false, true, false, false, false, false)
            }
            JokerType::GluttonousJoker => {
                JokerEffectType::new(false, true, false, false, false, false)
            }
            JokerType::JollyJoker => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::ZanyJoker => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::MadJoker => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::CrazyJoker => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::DrollJoker => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::SlyJoker => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::WilyJoker => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::CleverJoker => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::DeviousJoker => {
                JokerEffectType::new(true, false, false, false, false, false)
            }
            JokerType::CraftyJoker => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::HalfJoker => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::CreditCard => JokerEffectType::new(false, false, false, false, false, true),
            JokerType::Banner => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::MysticSummit => {
                JokerEffectType::new(false, true, false, false, false, false)
            }
            JokerType::EightBall => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Misprint => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::RaisedFist => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::ChaosTheClown => {
                JokerEffectType::new(false, false, false, true, false, false)
            }
            JokerType::ScaryFace => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::AbstractJoker => {
                JokerEffectType::new(false, true, false, false, false, false)
            }
            JokerType::DelayedGratification => {
                JokerEffectType::new(false, false, false, false, false, true)
            }
            JokerType::GrosMichel => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::EvenSteven => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::OddTodd => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::Scholar => JokerEffectType::new(true, true, false, false, false, false),
            JokerType::BusinessCard => {
                JokerEffectType::new(false, false, false, false, false, true)
            }
            JokerType::Supernova => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::RideTheBus => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::Egg => JokerEffectType::new(false, false, false, false, false, true),
            JokerType::Runner => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::IceCream => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::Splash => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::BlueJoker => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::FacelessJoker => {
                JokerEffectType::new(false, false, false, false, false, true)
            }
            JokerType::GreenJoker => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::Superposition => {
                JokerEffectType::new(false, false, false, true, false, false)
            }
            JokerType::ToDoList => JokerEffectType::new(false, false, false, false, false, true),
            JokerType::Cavendish => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::RedCard => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::SquareJoker => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::RiffRaff => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Photograph => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::ReservedParking => {
                JokerEffectType::new(false, false, false, false, false, true)
            }
            JokerType::MailInRebate => {
                JokerEffectType::new(false, false, false, false, false, true)
            }
            JokerType::Hallucination => {
                JokerEffectType::new(false, false, false, true, false, false)
            }
            JokerType::FortuneTeller => {
                JokerEffectType::new(false, true, false, false, false, false)
            }
            JokerType::Juggler => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Drunkard => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::GoldenJoker => JokerEffectType::new(false, false, false, false, false, true),
            JokerType::Popcorn => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::WalkieTalkie => JokerEffectType::new(true, true, false, false, false, false),
            JokerType::SmileyFace => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::GoldenTicket => {
                JokerEffectType::new(false, false, false, false, false, true)
            }
            JokerType::Swashbuckler => {
                JokerEffectType::new(false, true, false, false, false, false)
            }
            JokerType::HangingChad => JokerEffectType::new(false, false, false, false, true, false),
            JokerType::ShootTheMoon => {
                JokerEffectType::new(false, true, false, false, false, false)
            }
            JokerType::JokerStencil => {
                JokerEffectType::new(false, false, true, false, false, false)
            }
            JokerType::FourFingers => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Mime => JokerEffectType::new(false, false, false, false, true, false),
            JokerType::CeremonialDagger => {
                JokerEffectType::new(false, true, false, false, false, false)
            }
            JokerType::MarbleJoker => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::LoyaltyCard => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Dusk => JokerEffectType::new(false, false, false, false, true, false),
            JokerType::Fibonacci => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::SteelJoker => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Hack => JokerEffectType::new(false, false, false, false, true, false),
            JokerType::Pareidolia => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::SpaceJoker => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Burglar => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Blackboard => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::SixthSense => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Constellation => {
                JokerEffectType::new(false, false, true, false, false, false)
            }
            JokerType::Hiker => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::CardSharp => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Madness => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Seance => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Vampire => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Shortcut => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Hologram => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::CloudNine => JokerEffectType::new(false, false, false, false, false, true),
            JokerType::Rocket => JokerEffectType::new(false, false, false, false, false, true),
            JokerType::MidasMask => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Luchador => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::GiftCard => JokerEffectType::new(false, false, false, false, false, true),
            JokerType::TurtleBean => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Erosion => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::ToTheMoon => JokerEffectType::new(false, false, false, false, false, true),
            JokerType::StoneJoker => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::LuckyCat => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Bull => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::DietCola => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::TradingCard => JokerEffectType::new(false, false, false, false, false, true),
            JokerType::FlashCard => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::SpareTrousers => {
                JokerEffectType::new(false, true, false, false, false, false)
            }
            JokerType::Ramen => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Seltzer => JokerEffectType::new(false, false, false, false, true, false),
            JokerType::Castle => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::MrBones => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Acrobat => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::SockAndBuskin => {
                JokerEffectType::new(false, false, false, false, true, false)
            }
            JokerType::Troubadour => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Certificate => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::SmearedJoker => {
                JokerEffectType::new(false, false, false, true, false, false)
            }
            JokerType::Throwback => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::RoughGem => JokerEffectType::new(false, false, false, false, false, true),
            JokerType::Bloodstone => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Arrowhead => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::OnyxAgate => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::GlassJoker => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Showman => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::FlowerPot => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::MerryAndy => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::OopsAllSixes => {
                JokerEffectType::new(false, false, false, true, false, false)
            }
            JokerType::TheIdol => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::SeeingDouble => {
                JokerEffectType::new(false, false, true, false, false, false)
            }
            JokerType::Matador => JokerEffectType::new(false, false, false, false, false, true),
            JokerType::Satellite => JokerEffectType::new(false, false, false, false, false, true),
            JokerType::Cartomancer => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Astronomer => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Bootstraps => JokerEffectType::new(false, true, false, false, false, false),
            JokerType::Dna => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Vagabond => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Baron => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Obelisk => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::BaseballCard => {
                JokerEffectType::new(false, false, true, false, false, false)
            }
            JokerType::AncientJoker => {
                JokerEffectType::new(false, false, true, false, false, false)
            }
            JokerType::Campfire => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Blueprint => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::WeeJoker => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::HitTheRoad => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::TheDuo => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::TheTrio => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::TheFamily => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::TheOrder => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::TheTribe => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Stuntman => JokerEffectType::new(true, false, false, false, false, false),
            JokerType::InvisibleJoker => {
                JokerEffectType::new(false, false, false, true, false, false)
            }
            JokerType::Brainstorm => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::DriversLicense => {
                JokerEffectType::new(false, false, true, false, false, false)
            }
            JokerType::BurntJoker => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Canio => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Triboulet => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Yorick => JokerEffectType::new(false, false, true, false, false, false),
            JokerType::Chicot => JokerEffectType::new(false, false, false, true, false, false),
            JokerType::Perkeo => JokerEffectType::new(false, false, false, true, false, false),
        }
    }
}

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
