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
    #[derive(Serialize)]
    pub enum Enum {
        A(u8),
        B,
        C,
        D,
    }
    assert_eq!(<Enum as Serialize>::LEN, 5);
    assert_eq!(Serialize::serialize(&Enum::A(12)), [1., 0., 0., 0., 12.]);

    #[derive(Serialize)]
    pub struct Struct {
        a: u8,
        b: f32,
        c: [usize; 3],
        d: Enum,
    }
    assert_eq!(<Struct as Serialize>::LEN, 10);
    assert_eq!(
        Serialize::serialize(&Struct {
            a: 5,
            b: 32.,
            c: [0, 1, 2],
            d: Enum::A(38),
        }),
        [5., 32., 0., 1., 2., 1., 0., 0., 0., 38.]
    );
}
