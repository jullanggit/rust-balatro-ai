use crate::{
    Consumable, DeckCardIndices, Edition, Enhancement, Joker, PackState, PlayingCard, Seal,
    Sticker, Tag, Voucher, stackvec::StackVec,
};

pub type TensorElement = f32;

pub trait Serialize
where
    Self: Sized,
{
    const LEN: usize;
    fn serialize(&self) -> [TensorElement; Self::LEN];
    fn deserialize(value: &[TensorElement; Self::LEN]) -> Option<Self>;
}

macro_rules! impl_serialize_with_as {
    ($($t:ty),+) => {
        $(
            impl Serialize for $t {
                const LEN: usize = 1;

                fn serialize(&self) -> [TensorElement; Self::LEN] {
                    [*self as TensorElement]
                }
                fn deserialize(value: &[TensorElement; Self::LEN]) -> Option<Self> {
                    Some(value[0] as $t)
                }
            }
        )+
    };
}

impl_serialize_with_as!(
    u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize, f32, f64
);

impl Serialize for bool {
    const LEN: usize = 1;
    fn serialize(&self) -> [TensorElement; Self::LEN] {
        [*self as u8 as TensorElement]
    }
    fn deserialize(value: &[TensorElement; Self::LEN]) -> Option<Self> {
        match value[0] {
            0. => Some(false),
            1. => Some(true),
            _ => None,
        }
    }
}

macro_rules! impl_serialize_for_array {
    ($($t:ty),+) => {
        $(
            impl<const LEN: usize> Serialize for [$t; LEN] {
                const LEN: usize = <$t>::LEN * LEN;

                fn serialize(&self) -> [TensorElement; Self::LEN] {
                    let mut initial = [0.; Self::LEN];

                    for (i, elem) in self.iter().enumerate() {
                        let len = <$t>::LEN;
                        initial[i * len..(i + 1) * len].copy_from_slice(&elem.serialize());
                    }

                    initial
                }
                fn deserialize(value: &[TensorElement; Self::LEN]) -> Option<Self> {
                    core::array::try_from_fn(|i| {
                        let len = <$t>::LEN;
                        <$t>::deserialize(value[i * len.. (i+1) * len].try_into().unwrap())
                    })
                }
            }
        )+
    };
}
impl_serialize_for_array!(u8, usize, Tag);

// handle non-present elements
macro_rules! impl_serialize_for_stackvec {
    ($($t:ty),+) => {
        $(
            // encode by prepending a binary 'present' flag before every element
            impl<const LEN: usize> Serialize for StackVec<$t, LEN> {
                const LEN: usize = (<$t>::LEN + 1) * LEN; // LEN + 1 to account for 'present' flag

                fn serialize(&self) -> [TensorElement; Self::LEN] {
                    let mut initial = [0.; Self::LEN];

                    let len = <$t>::LEN + 1; // + 1 to account for 'present' flag

                    for (i, elem) in self.iter().enumerate() {
                        initial[i * len] = 1.;
                        initial[i * len + 1..(i + 1) * len].copy_from_slice(&elem.serialize());
                    }

                    initial
                }
                fn deserialize(value: &[TensorElement; Self::LEN]) -> Option<Self> {
                    let len = <$t>::LEN + 1;

                    let mut out = Self::new();
                    for i in 0..(Self::LEN / len) {
                        if value[i * len] == 1. {
                            out.push(<$t>::deserialize(value[i * len + 1..(i + 1) * len].try_into().unwrap())?)
                            .expect("'value' has the exact length (Self::LEN) needed to fit all elements");
                        }
                    }

                    Some(out)
                }
            }
        )+
    };
}
impl_serialize_for_stackvec!(usize, u8, Voucher, Tag, PlayingCard, Joker, Consumable);

#[test]
fn test_stackvec_serialize() {
    use crate::{Edition, JokerType};

    let mut to_serialize = StackVec::<_, 5>::new();

    let joker = Joker {
        joker_type: JokerType::Acrobat,
        edition: Some(Edition::Holographic),
        sticker: None,
    };

    for joker in core::iter::repeat_n(joker, 4) {
        to_serialize.push(joker).unwrap();
    }

    let serialized = Serialize::serialize(&to_serialize);

    assert_eq!(to_serialize, Serialize::deserialize(&serialized).unwrap());
}

macro_rules! impl_serialize_for_tuple {
    ($(($t1:ty, $t2:ty)),+) => {
        $(
            impl Serialize for ($t1, $t2) {
                const LEN: usize = <$t1>::LEN + <$t2>::LEN;

                fn serialize(&self) -> [TensorElement; Self::LEN] {
                    let mut initial = [0.; Self::LEN];

                    initial[0..<$t1>::LEN].copy_from_slice(&self.0.serialize());
                    initial[<$t1>::LEN..<$t1>::LEN+<$t2>::LEN].copy_from_slice(&self.1.serialize());

                    initial
                }
                fn deserialize(value: &[TensorElement; Self::LEN]) -> Option<Self> {
                    Some(
                    (Serialize::deserialize(&value[0..<$t1>::LEN].try_into().unwrap())?,
                    Serialize::deserialize(&value[<$t1>::LEN..<$t1>::LEN+<$t2>::LEN].try_into().unwrap())?)
                    )
                }
            }
        )+
    };
}
impl_serialize_for_tuple!((usize, DeckCardIndices));

macro_rules! impl_serialize_for_option {
    ($($t:ty),+) => {
        $(
            impl Serialize for Option<$t> {
                const LEN: usize = 2 + <$t>::LEN;

                fn serialize(&self) -> [TensorElement; Self::LEN] {
                    let mut initial = [0.; Self::LEN];

                    match self {
                        None => {
                            initial[0] = 1.;
                        }
                        Some(data) => {
                            initial[1] = 1.;
                            initial[2..].copy_from_slice(&data.serialize());
                        }
                    }

                    initial
                }
                fn deserialize(value: &[TensorElement; Self::LEN]) -> Option<Self> {
                    match [value[0], value[1]] {
                        [1., 0.] => Some(None),
                        [0., 1.] => Some(Some(Serialize::deserialize(value[2..].try_into().unwrap())?)),
                        _ => None
                    }
                }
            }
        )+
    };
}
impl_serialize_for_option!(Edition, PackState, Sticker, Enhancement, Seal);
