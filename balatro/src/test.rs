use fastrand::Rng;

use crate::{
    BossBlind,
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
    #[derive(Debug, PartialEq)]
    #[Serialize(encode_me)]
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

    #[derive(Debug, PartialEq)]
    #[Serialize(also_encode)]
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
