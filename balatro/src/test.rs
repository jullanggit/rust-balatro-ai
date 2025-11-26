use fastrand::Rng;
use proc_macros::Serialize;

use crate::{
    BossBlind, Hand, MAX_PLAYED_HAND_CARDS, PlayingCard, PlayingCardRank, PlayingCardSuit,
    serialize::{Serialize, TensorElement},
};

#[test]
fn test_random_bossblind() {
    let mut rng = Rng::with_seed(123);
    for _ in 0..1000 {
        let ante = rng.u8(1..8);
        let random = BossBlind::random(&mut rng, ante);
        assert!(random as u8 <= BossBlind::NUM_VARIANTS_AT_ANTE.into_iter().sum());
    }
}

#[test]
fn test_serialization() {
    #[derive(Debug, PartialEq, Serialize)]
    #[serialize(encode_me)]
    pub enum Enum {
        A(u8),
        B,
        C,
        D,
    }
    impl Enum {
        fn encode_me(&self) -> u8 {
            3
        }
    }
    assert_eq!(<Enum as Serialize>::LEN, 6);
    let start = Enum::A(12);
    let serialized = Serialize::serialize(&start);
    assert_eq!(serialized, [1., 0., 0., 0., 12., 3.]);
    assert_eq!(Serialize::deserialize(&serialized), Some(start));

    #[derive(Debug, PartialEq, Serialize)]
    #[serialize(also_encode)]
    pub struct Struct {
        a: u8,
        b: f32,
        c: [usize; 3],
        d: Enum,
    }
    impl Struct {
        fn also_encode(&self) -> f32 {
            self.b.sqrt()
        }
    }
    assert_eq!(<Struct as Serialize>::LEN, 12);
    let start = Struct {
        a: 5,
        b: 36.,
        c: [0, 1, 2],
        d: Enum::A(38),
    };
    let serialized = Serialize::serialize(&start);
    assert_eq!(
        serialized,
        [5., 36., 0., 1., 2., 1., 0., 0., 0., 38., 3., 6.]
    );
    assert_eq!(Serialize::deserialize(&serialized), Some(start));
}

fn test_playing_card_hand() {
    use PlayingCardRank::*;
    use PlayingCardSuit::*;
    let cards = [
        (
            [
                (Hearts, Ace),
                (Clubs, Two),
                (Hearts, Three),
                (Diamonds, Four),
                (Diamonds, Five),
            ],
            (Hand::Straight, [true; MAX_PLAYED_HAND_CARDS]),
        ),
        (
            [
                (Hearts, Ace),
                (Hearts, Two),
                (Hearts, Three),
                (Hearts, Four),
                (Hearts, Five),
            ],
            (Hand::StraightFlush, [true; MAX_PLAYED_HAND_CARDS]),
        ),
        (
            [
                (Hearts, Ace),
                (Hearts, King),
                (Hearts, Five),
                (Hearts, Four),
                (Hearts, Five),
            ],
            (Hand::Flush, [true; MAX_PLAYED_HAND_CARDS]),
        ),
        (
            [
                (Hearts, Ace),
                (Hearts, Ace),
                (Spades, Five),
                (Hearts, Four),
                (Clubs, Five),
            ],
            (Hand::TwoPair, [true, true, true, false, true]),
        ),
        (
            [
                (Hearts, Ace),
                (Hearts, Ace),
                (Spades, Five),
                (Hearts, Five),
                (Clubs, Five),
            ],
            (Hand::FullHouse, [true; MAX_PLAYED_HAND_CARDS]),
        ),
        (
            [
                (Hearts, Ace),
                (Hearts, Ace),
                (Spades, Ace),
                (Hearts, Ace),
                (Clubs, Ace),
            ],
            (Hand::FiveOfAKind, [true; MAX_PLAYED_HAND_CARDS]),
        ),
        (
            [
                (Hearts, King),
                (Hearts, Ace),
                (Spades, Four),
                (Hearts, Three),
                (Clubs, Jack),
            ],
            (Hand::HighCard, [false, true, false, false, false]),
        ),
    ];
    for (cards, expected) in cards {
        let cards = cards.map(|card| card.into());
        assert_eq!(PlayingCard::hand(&cards.into_iter().collect()), expected);
    }
}
