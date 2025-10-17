use crate::{
    Consumable, Edition, Enhancement, Joker, MAX_DECK_CARDS, PackState, PlayingCard, Seal, Sticker,
    Tag, Voucher, stackvec::StackVec,
};

pub type TensorElement = f32;

pub trait Serialize {
    const LEN: usize;
    fn serialize(&self) -> [TensorElement; Self::LEN];
}

macro_rules! Serialize {
    derive() (
        $(#[repr(u8)])?
        $(#[doc = $_l1:literal])?
        $(pub)? enum $Enum:ident {
            $(
                $(#[doc = $_l2:literal])?
                $(#[default])?
                $Variant:ident $(($Data:ident))?,
            )*
    }) => {
        impl Serialize for $Enum {
            const LEN: usize = {
                let mut max = 0;
                $($(
                    let len = <$Data as Serialize>::LEN;
                    if len > max {
                        max = len;
                    }
                )?)*

                max + core::mem::variant_count::<$Enum>()
            };

            #[expect(non_snake_case)]
            fn serialize(&self) -> [TensorElement; Self::LEN] {
                let mut initial = [0.0; _];

                fn discriminant(value: &$Enum) -> usize {
                    let mut num = 0;
                    $(
                        if matches!(value, $Enum::$Variant $(($Data))?) {
                            return num;
                        } else {
                            num += 1;
                        }
                    )*

                    unreachable!()
                }

                initial[discriminant(self)] = 1.0;

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

                initial
            }
        }
    };
    derive() (
        $(#[doc = $_l1:literal])?
        $(pub)? struct $Struct:ident {
            $(
                $(#[doc = $_l2:literal])?
                $vis:vis $field:ident: $ty:ty,
            )*
    }) => {
        impl Serialize for $Struct {
            const LEN: usize = $(<$ty as Serialize>::LEN +)* 0;

            fn serialize(&self) -> [TensorElement; Self::LEN] {
                let mut initial = [0.0; _];

                let mut offset = 0;

                $(
                    let len = <$ty as Serialize>::LEN;
                    initial[offset..offset + len]
                            .copy_from_slice(&<$ty as Serialize>::serialize(&self.$field));

                    offset += len;
                )*

                initial
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
}

macro_rules! impl_serialize_for_array {
    ($($t:ty),+) => {
        $(
            impl<const LEN: usize> Serialize for [$t; LEN] {
                const LEN: usize = <$t>::LEN * LEN;

                fn serialize(&self) -> [TensorElement; Self::LEN] {
                    let mut initial = [0.; Self::LEN];

                    for (i, elem) in self.iter().enumerate() {
                        initial[i * <$t>::LEN..i * <$t>::LEN + <$t>::LEN].copy_from_slice(&elem.serialize());
                    }

                    initial
                }
            }
        )+
    };
}
impl_serialize_for_array!(usize, Tag);

macro_rules! impl_serialize_for_stackvec {
    ($($t:ty),+) => {
        $(
            impl<const LEN: usize> Serialize for StackVec<$t, LEN> {
                const LEN: usize = <$t>::LEN * LEN;

                fn serialize(&self) -> [TensorElement; Self::LEN] {
                    let mut initial = [0.; Self::LEN];

                    for (i, elem) in self.iter().enumerate() {
                        initial[i * <$t>::LEN..i * <$t>::LEN + <$t>::LEN].copy_from_slice(&elem.serialize());
                    }

                    initial
                }
            }
        )+
    };
}
impl_serialize_for_stackvec!(usize, u8, Voucher, Tag, PlayingCard, Joker, Consumable);

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
            }
        )+
    };
}
impl_serialize_for_tuple!((usize, StackVec<usize, MAX_DECK_CARDS>));

macro_rules! impl_serialize_for_option {
    ($($t:ty),+) => {
        $(
            impl Serialize for Option<$t> {
                const LEN: usize = 2 + <$t>::LEN;

                fn serialize(&self) -> [TensorElement; Self::LEN] {
                    let mut initial = [0.; Self::LEN];

                    match self {
                        Some(data) => {
                            initial[0] = 1.;
                            initial[2..].copy_from_slice(&data.serialize());
                        }
                        None => {
                            initial[1] = 1.;
                        }
                    }

                    initial
                }
            }
        )+
    };
}
impl_serialize_for_option!(Edition, PackState, Sticker, Enhancement, Seal);
