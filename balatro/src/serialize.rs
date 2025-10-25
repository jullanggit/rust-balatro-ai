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

macro_rules! Serialize {
    attr($($fn:ident),*) (
        $(#[repr(u8)])?
        $(#[doc = $_l1:literal])?
        $pub:vis enum $Enum:ident {
            $(
                $(#[doc = $_l2:literal])?
                $(#[default])?
                $Variant:ident $(($Data:ident))?,
            )*
    }) => {
        $pub enum $Enum {
            $(
                $Variant $(($Data))?,
            )*
        }

        mod ${concat(__, $Enum, _serialization)} {
            use super::*;

            pub const fn max_len() -> usize {
                #[allow(unused_mut)]
                let mut max = 0;
                $($(
                    let len = <$Data as Serialize>::LEN;
                    if len > max {
                        max = len;
                    }
                )?)*

                max
            }
        }

        impl Serialize for $Enum {
            const LEN: usize = {
                let max = ${concat(__, $Enum, _serialization)}::max_len();

                // uses generics for the return types of the functions, to let the type system infer them
                const fn get_fns_len<$(${concat($fn, _type)}: Serialize),*>
                    ($($fn: fn (&$Enum) -> ${concat($fn, _type)}),*) -> usize
                {
                    $(${concat($fn, _type)}::LEN + )* 0
                }

                core::mem::variant_count::<$Enum>() + max + get_fns_len($($Enum::$fn),*)
            };

            #[allow(non_snake_case)]
            fn serialize(&self) -> [TensorElement; Self::LEN] {
                let mut initial = [0.0; _];

                #[allow(unused_assignments)] // the last num += 1
                fn discriminant(value: &$Enum) -> usize {
                    let mut num = 0;
                    $(
                        #[allow(unused_variables)]
                        if matches!(value, $Enum::$Variant $(($Data))?) {
                            return num;
                        } else {
                            num += 1;
                        }
                    )*

                    unreachable!()
                }

                initial[discriminant(self)] = 1.0;

                #[allow(unused_mut)]
                let mut offset = core::mem::variant_count::<$Enum>();

                match self {
                    $(
                        $Enum::$Variant $(($Data))? => {
                            $(
                                initial[offset..offset+<$Data as Serialize>::LEN]
                                    .copy_from_slice(&<$Data as Serialize>::serialize($Data));
                            )?
                        }
                    )*
                }

                // uses generics for the return types of the functions, to let the type system infer them
                fn encode_fns<$(${concat($fn, _type)}: Serialize),*>
                    (value: &$Enum, mut offset: usize, out: &mut [TensorElement; $Enum::LEN], $($fn: fn (&$Enum) -> ${concat($fn, _type)}),*)
                {
                    $(
                        let len = ${concat($fn, _type)}::LEN;

                        out[offset..offset + len].copy_from_slice(&Serialize::serialize(&value.$fn()));

                        offset += len;
                    )*
                }
                encode_fns(self, offset + ${concat(__, $Enum, _serialization)}::max_len(), &mut initial, $($Enum::$fn),*);

                initial
            }
            fn deserialize(value: &[TensorElement; Self::LEN]) -> Option<Self> {
                let mut i = 0;

                $(
                    if value[i] == 1. {
                        let offset = core::mem::variant_count::<$Enum>();
                        return Some(
                            $Enum::$Variant $(
                                // TODO: maybe do this without round-trip to slices and expect()
                                ($Data::deserialize(&value[offset..offset + $Data::LEN].try_into().expect("Lengths should match"))?)
                            )?
                        )
                    }
                    i += 1;
                )*

                None
            }
        }
    };
    attr($($fn:ident),*) (
        $(#[doc = $_l1:literal])?
        $pub:vis struct $Struct:ident {
            $(
                $(#[doc = $_l2:literal])?
                $vis:vis $field:ident: $ty:ty,
            )*
    }) => {
        $pub struct $Struct {
            $(
                $vis $field: $ty,
            )*
        }

        impl Serialize for $Struct {
            const LEN: usize = {
                // uses generics for the return types of the functions, to let the type system infer them
                const fn get_fns_len<$(${concat($fn, _type)}: Serialize),*>
                    ($($fn: fn (&$Struct) -> ${concat($fn, _type)}),*) -> usize
                {
                    $(${concat($fn, _type)}::LEN + )* 0
                }

                $(<$ty as Serialize>::LEN +)* get_fns_len($($Struct::$fn),*)
            };

            fn serialize(&self) -> [TensorElement; Self::LEN] {
                let mut initial = [0.0; _];

                let mut offset = 0;

                $(
                    let len = <$ty as Serialize>::LEN;
                    initial[offset..offset + len]
                            .copy_from_slice(&<$ty as Serialize>::serialize(&self.$field));

                    offset += len;
                )*

                // uses generics for the return types of the functions, to let the type system infer them
                fn encode_fns<$(${concat($fn, _type)}: Serialize),*>
                    (value: &$Struct, mut offset: usize, out: &mut [TensorElement; $Struct::LEN], $($fn: fn (&$Struct) -> ${concat($fn, _type)}),*)
                {
                    $(
                        let len = ${concat($fn, _type)}::LEN;

                        out[offset..offset + len].copy_from_slice(&Serialize::serialize(&value.$fn()));

                        offset += len;
                    )*
                }
                encode_fns(self, offset, &mut initial, $($Struct::$fn),*);

                initial
            }
            fn deserialize(value: &[TensorElement; Self::LEN]) -> Option<Self> {
                let mut offset = 0;

                Some(Self {
                    $(
                        $field: {
                            let len = <$ty as Serialize>::LEN;
                            let val = <$ty>::deserialize(&value[offset..offset + len].try_into().unwrap())?;
                            offset += len;

                            val
                        },
                    )*
                })
            }
        }
    };
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
impl_serialize_for_array!(usize, Tag);

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
