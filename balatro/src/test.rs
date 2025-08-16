use fastrand::Rng;

use crate::BossBlind;

#[test]
fn test_random_bossblind() {
    let mut rng = Rng::new();
    for _ in 0..1000 {
        let ante = rng.u8(1..8);
        let random = BossBlind::random(&mut rng, ante);
        assert!(random as u8 <= BossBlind::NUM_VARIANTS_AT_ANTE.into_iter().sum());
    }
}
