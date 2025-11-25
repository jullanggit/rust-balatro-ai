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
