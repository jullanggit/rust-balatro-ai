// no_std to make running on the gpu possible in the future
#![no_std]
#![feature(maybe_uninit_uninit_array_transpose)]
#![feature(maybe_uninit_slice)]
#![feature(transmutability)]
#![feature(generic_const_exprs)]
#![feature(variant_count)]
#![feature(macro_metavar_expr_concat)]
#![feature(array_try_from_fn)]
#![feature(array_windows)]

use crate::stackvec::{Len, StackVec};
use core::{
    array,
    mem::{self, Assume, TransmuteFrom},
    ops::{Deref, Index, IndexMut},
};
use fastrand::Rng;
use proc_macros::Serialize;
use serialize::{Serialize, TensorElement};

#[macro_use]
mod serialize;
pub mod stackvec;
#[cfg(test)]
mod test;

const MAX_CONSUMABLES: usize = 25;
const MAX_PLAYED_HAND_CARDS: usize = 5;
const MAX_DECK_CARDS: usize = 100;
const MAX_JOKERS: usize = 15;
const MAX_HAND_CARDS: usize = 20;
const MAX_TAGS: usize = 20;
const MAX_SHOP_CARDS: usize = 4;
const MAX_SHOP_VOUCHERS: usize = 5;
const MAX_SHOP_PACKS: usize = 2;
const MAX_PACK_ITEMS: usize = 5;

#[derive(Debug)]
pub struct Game {
    state: GameState,
    rng: Rng,
}
impl Game {
    /// Result<end of game, error>
    pub fn execute_action(&mut self, action: Action) -> Option<bool> {
        // take temporary ownership of GameState, will be released at the end of the function
        let mut state = mem::take(&mut self.state);

        let res = 'res: {
            let mut end_of_game = false;

            match action {
                Action::SelectDeck(deck) => {
                    if let GameState::SelectingDeck = state {
                        state = GameState::SelectingStake(deck);
                    } else {
                        break 'res None;
                    }
                }
                Action::SelectStake(stake) => {
                    match state {
                        GameState::SelectingStake(deck) => {
                            state = GameState::SelectingBlind(SelectingBlind {
                                in_game: InGame {
                                    deck_cards: deck.deck_cards(&mut self.rng),
                                    deck,
                                    stake,
                                    blind_progress: BlindProgress::Small,
                                    boss_blind: BossBlind::random(&mut self.rng, 1),
                                    blind_tags: [Tag::Uncommon; 2],
                                    money: 4,
                                    // TODO: adjust based on decks
                                    hands: 4,
                                    discards: 3,
                                    jokers: StackVec::new(),
                                    consumables: StackVec::new(),
                                    tags_to_use: StackVec::new(),
                                    vouchers: StackVec::new(),
                                    ante: 1,
                                    hand_size: 8,
                                    hand_levels: HandLevels([1; _]),
                                    joker_slots: 5,
                                },
                                pack: None,
                            });
                        }
                        _ => break 'res None,
                    }
                }
                Action::SelectBlind => match state {
                    // disallow selecting blind while opening pack
                    GameState::SelectingBlind(SelectingBlind {
                        mut in_game,
                        pack: None,
                        ..
                    }) => {
                        // advance blind progress and enter round
                        in_game.blind_progress.advance(&mut in_game.ante);

                        let mut in_round = InRound {
                            hand_card_indices: StackVec::new(),
                            used_card_indices: StackVec::new(),
                            discards_remaining: 3,
                            in_game,
                        };

                        in_round.draw_cards(in_round.in_game.hand_size, &mut self.rng);

                        if in_round.hand_card_indices.is_empty() {
                            end_of_game = true;
                        }

                        state = GameState::InRound(in_round);
                    }
                    _ => break 'res None,
                },
                Action::SkipBlind => match state {
                    // disallow skipping blind while opening pack
                    GameState::SelectingBlind(SelectingBlind {
                        ref mut in_game,
                        pack: None,
                        ..
                    }) => {
                        in_game.blind_progress.advance(&mut in_game.ante);
                    }
                    _ => break 'res None,
                },
                Action::RerollBossBlind => {
                    match state {
                        // disallow rerolling boss blind while opening pack
                        GameState::SelectingBlind(SelectingBlind {
                            ref mut in_game,
                            pack: None,
                            ..
                        }) => {
                            let mut can_reroll = false;
                            if in_game.money >= 10 {
                                for voucher in in_game.vouchers.iter_mut() {
                                    if let Voucher::DirectorsCut(false) = voucher {
                                        *voucher = Voucher::DirectorsCut(true);
                                        can_reroll = true;
                                        break;
                                    }
                                    if let Voucher::Retcon = voucher {
                                        can_reroll = true;
                                        break;
                                    }
                                }
                            }
                            if can_reroll {
                                // reroll
                                in_game.money -= 10;
                                in_game.boss_blind = BossBlind::random(&mut self.rng, in_game.ante);
                            } else {
                                break 'res None;
                            }
                        }
                        _ => break 'res None,
                    }
                }
                Action::PlayHand(hand_card_indices) => match &mut state {
                    GameState::InRound(in_round) => {
                        // - remove played cards from hand cards
                        // - TODO: rename discarded cards to used cards
                        // - add them to used cards
                        // - score
                        // - draw new cards

                        assert!(
                            hand_card_indices
                                .iter()
                                .all(|index| in_round.hand_card_indices.contains(index))
                        );

                        // round entered jokers
                        for i in 0..in_round.in_game.jokers.len() {
                            // first clone joker...
                            if let Some(mut joker) = in_round.in_game.jokers.get(i).cloned() {
                                // ...then potentially remove the next joker...
                                joker.on_round_entered(&mut in_round.in_game, i);
                                // ...then update the current one
                                in_round.in_game.jokers[i] = joker;
                            }
                        }

                        let mut played_cards: StackVec<PlayingCard, MAX_PLAYED_HAND_CARDS> =
                            StackVec::new();
                        for index in hand_card_indices.iter() {
                            played_cards
                                .push(in_round.in_game.deck_cards[*index as usize].clone())
                                .unwrap();

                            InRound::mark_hand_card_as_used(
                                &mut in_round.hand_card_indices,
                                &mut in_round.used_card_indices,
                                *index,
                            )
                            .expect("We check that all indices are contained");
                        }

                        let (hand, included) = PlayingCard::hand(&played_cards);
                        let (mut chips, mut mult) = in_round.in_game.hand_levels.chips_mult(hand);

                        // hand played jokers
                        for joker in in_round.in_game.jokers.clone().iter() {
                            joker.on_hand_played(
                                hand,
                                &mut chips,
                                &mut mult,
                                &mut in_round.in_game,
                            );
                        }

                        // scoring cards
                        for index in included
                            .into_iter()
                            .enumerate()
                            .filter(|(_, included)| *included)
                            .map(|(index, _)| index)
                            // stone cards
                            .chain(
                                played_cards
                                    .iter()
                                    .enumerate()
                                    // that arent already scored
                                    .filter(|(index, _)| !included[*index])
                                    .filter(|(_, card)| {
                                        card.enhancement == Some(Enhancement::Stone)
                                    })
                                    .map(|(index, _)| index),
                            )
                        {
                            let card = &played_cards[index];
                            card.on_score(&mut chips, &mut mult);

                            for i in 0..in_round.in_game.jokers.len() {
                                if let Some(joker) = in_round.in_game.jokers.get(i) {
                                    joker.on_card_scored(card, hand, &mut chips, &mut mult);
                                }
                            }
                        }

                        todo!();
                    }
                    _ => break 'res None,
                },
                Action::DiscardHand(discard_indices) => {
                    match &mut state {
                        GameState::InRound(in_round) if in_round.discards_remaining > 0 => {
                            // move cards from hand to discarded
                            for index in discard_indices.iter() {
                                // TODO: purple seal
                                let Some(()) = InRound::mark_hand_card_as_used(
                                    &mut in_round.hand_card_indices,
                                    &mut in_round.used_card_indices,
                                    *index,
                                ) else {
                                    break 'res None;
                                };
                            }
                            // draw new cards
                            let num_to_draw =
                                if in_round.in_game.boss_blind == BossBlind::TheSerpent {
                                    3
                                } else {
                                    discard_indices.len() as u8
                                };
                            in_round.draw_cards(num_to_draw, &mut self.rng);

                            if in_round.hand_card_indices.is_empty() {
                                end_of_game = true;
                            }
                        }
                        _ => break 'res None,
                    }
                }
                Action::MoveJoker([current_position, new_position]) => match state {
                    GameState::SelectingBlind(SelectingBlind {
                        in_game: InGame { ref mut jokers, .. },
                        ..
                    })
                    | GameState::InRound(InRound {
                        in_game: InGame { ref mut jokers, .. },
                        ..
                    })
                    | GameState::CashingOut(CashingOut {
                        in_game: InGame { ref mut jokers, .. },
                    })
                    | GameState::InShop(InShop {
                        in_game: InGame { ref mut jokers, .. },
                        ..
                    }) => {
                        jokers.relocate(current_position, new_position);
                    }
                    _ => break 'res None,
                },
                Action::SellJoker(index) | Action::SellConsumable(index) => match state {
                    GameState::SelectingBlind(SelectingBlind {
                        ref mut in_game, ..
                    })
                    | GameState::InRound(InRound {
                        ref mut in_game, ..
                    })
                    | GameState::CashingOut(CashingOut { ref mut in_game })
                    | GameState::InShop(InShop {
                        ref mut in_game, ..
                    }) => {
                        fn sell<T: Price, const N: usize>(
                            cost_multiplier: f32,
                            collection: &mut StackVec<T, N>,
                            index: usize,
                            money: &mut i32,
                        ) -> Option<()> {
                            let item = collection.remove(index)?;
                            *money += (item.sell_price() as f32 * cost_multiplier) as i32;

                            Some(())
                        }
                        match action {
                            Action::SellJoker(_) => sell(
                                in_game.cost_multiplier(),
                                &mut in_game.jokers,
                                index,
                                &mut in_game.money,
                            ),
                            Action::SellConsumable(_) => sell(
                                in_game.cost_multiplier(),
                                &mut in_game.consumables,
                                index,
                                &mut in_game.money,
                            ),
                            _ => unreachable!(),
                        };
                    }
                    _ => break 'res None,
                },
                Action::UseConsumable(_) => todo!(),
                Action::BuyShopCard(_) => todo!(),
                Action::RedeemVoucher(_) => todo!(),
                Action::OpenPack(_) => todo!(),
                Action::Reroll => todo!(),
                Action::NextRound => match state {
                    // disallow leaving shop while opening pack
                    GameState::InShop(InShop {
                        in_game,
                        pack: None,
                        ..
                    }) => {
                        state = GameState::SelectingBlind(SelectingBlind {
                            in_game,
                            pack: None,
                        })
                    }
                    _ => break 'res None,
                },
                Action::ChoosePackItem(_) => todo!(),
                Action::SkipPack => match state {
                    GameState::SelectingBlind(SelectingBlind { ref mut pack, .. })
                    | GameState::InShop(InShop { ref mut pack, .. })
                        if pack.is_some() =>
                    {
                        *pack = None;
                    }
                    _ => break 'res None,
                },
            }

            Some(end_of_game)
        };

        // give back ownership
        self.state = state;
        res
    }
}

#[derive(Debug, Default, Serialize)]
pub enum GameState {
    #[default]
    SelectingDeck,
    SelectingStake(Deck),
    SelectingBlind(SelectingBlind),
    InRound(InRound),
    CashingOut(CashingOut),
    InShop(InShop),
}

/// State that is present during blind selection
#[derive(Debug, Serialize)]
pub struct SelectingBlind {
    in_game: InGame,
    pack: Option<PackState>,
}
/// State that is present in a round
#[derive(Debug, Serialize)]
pub struct InRound {
    in_game: InGame,
    hand_card_indices: HandCardIndices,
    used_card_indices: DeckCardIndices,
    discards_remaining: u8,
}
impl InRound {
    #[must_use = "Should handle error"]
    /// Draw a card from the remaining deck into the hand. Returns None if the remaining deck is empty.
    fn draw_card(&mut self, rng: &mut Rng) -> Option<()> {
        // all indices that arent already in hand or discarded
        let in_deck_indices: StackVec<u8, MAX_DECK_CARDS> = (0..self.in_game.deck_cards.len()
            as u8)
            .filter(|index| {
                !(self.hand_card_indices.contains(index) || self.used_card_indices.contains(index))
            })
            .collect();

        let range = 0..in_deck_indices.len() as u8;
        if range.is_empty() {
            return None;
        }

        let indices_index = rng.u8(range);
        self.hand_card_indices
            .push(in_deck_indices[indices_index as usize])
            .expect("Max hand cards should not be reached");

        Some(())
    }
    /// Draw `num` cards. Returns how many draw where successful
    fn draw_cards(&mut self, num: u8, rng: &mut Rng) -> u8 {
        for i in 0..num {
            if self.draw_card(rng).is_none() {
                return i;
            }
        }
        num
    }
    /// Returns none if index is not in hand
    fn mark_hand_card_as_used(
        hand_card_indices: &mut HandCardIndices,
        discarded_card_indices: &mut DeckCardIndices,
        index: u8,
    ) -> Option<()> {
        let item = hand_card_indices.remove(index as usize)?;
        discarded_card_indices.push(item).expect(
            "Capacities of deck_cards and discarded_card_indices match, so item should always fit",
        );

        Some(())
    }
}
/// State that is present while cashing out
#[derive(Debug, Serialize)]
pub struct CashingOut {
    in_game: InGame,
}
/// State that is present in the Shop
#[derive(Debug, Serialize)]
pub struct InShop {
    in_game: InGame,
    pack: Option<PackState>,
}
/// State that is present after selecting Blind and Stake
#[derive(Debug, Serialize)]
#[serialize(cost_multiplier)]
pub struct InGame {
    deck: Deck,
    stake: Stake,
    blind_progress: BlindProgress,
    boss_blind: BossBlind,
    blind_tags: [Tag; 2],
    money: i32,
    hands: u8,
    discards: u8,
    jokers: StackVec<Joker, MAX_JOKERS>,
    joker_slots: u8,
    consumables: StackVec<Consumable, MAX_CONSUMABLES>,
    vouchers: StackVec<Voucher, MAX_CONSUMABLES>,
    tags_to_use: StackVec<Tag, MAX_TAGS>,
    ante: Ante,
    deck_cards: DeckCards,
    hand_size: u8,
    hand_levels: HandLevels,
}
impl InGame {
    fn cost_multiplier(&self) -> f32 {
        if self.vouchers.contains(&Voucher::Liquidation) {
            0.5
        } else if self.vouchers.contains(&Voucher::ClearanceSale) {
            0.75
        } else {
            1.
        }
    }
}

// TODO
#[derive(Debug, PartialEq, Serialize)]
pub struct PackState {}

type Ante = u8;
type DeckCards = StackVec<PlayingCard, MAX_DECK_CARDS>;
type DeckCardIndices = StackVec<u8, MAX_DECK_CARDS>;
type HandCardIndices = StackVec<u8, MAX_HAND_CARDS>;
type PlayedHandCardIndices = StackVec<u8, MAX_PLAYED_HAND_CARDS>;
type UseConsumable = (usize, DeckCardIndices);
type PackIndices = StackVec<usize, MAX_PACK_ITEMS>;
type MoveJoker = [usize; 2];

#[derive(Debug, Serialize)]
pub enum Action {
    SelectDeck(Deck),
    SelectStake(Stake),
    SelectBlind,
    SkipBlind,
    RerollBossBlind,
    /// Play the hand containing the cards at the indices contained in the vec, in the order of the vec
    PlayHand(PlayedHandCardIndices),
    /// Discard the hand containing the cards at the indices contained in the vec
    DiscardHand(PlayedHandCardIndices),
    /// [current position, new position]
    MoveJoker(MoveJoker),
    SellJoker(usize),
    /// [consumable index, hand cards to operate on (in the order present in the vec)]
    UseConsumable(UseConsumable),
    SellConsumable(usize),
    BuyShopCard(usize),
    RedeemVoucher(usize),
    OpenPack(usize),
    Reroll,
    NextRound,
    ChoosePackItem(PackIndices),
    SkipPack,
}

pub trait Price {
    fn buy_price(&self) -> u8;
    fn sell_price(&self) -> u8 {
        1.max(self.buy_price() / 2)
    }
}

pub trait Name {
    fn name(&self) -> &'static str;
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serialize(buy_price, sell_price)]
pub struct Joker {
    joker_type: JokerType,
    edition: Option<Edition>,
    sticker: Option<Sticker>,
}
impl Price for Joker {
    fn buy_price(&self) -> u8 {
        self.joker_type.buy_price() + self.edition.as_ref().map_or(0, Price::buy_price)
    }
}
// TODO:
// - Four Fingers
// - Mime
// - Credit Card
// - Dusk
impl Joker {
    fn on_round_entered(&mut self, in_game: &mut InGame, current_joker_index: usize) {
        use JokerType::*;

        match &mut self.joker_type {
            CeremonialDagger(mult) => {
                let removed_joker = in_game.jokers.remove(current_joker_index + 1);
                if let Some(joker) = removed_joker {
                    *mult += joker.sell_price() as u16
                }
            }
            MarbleJoker => {
                let _ = in_game.deck_cards.push(PlayingCard {
                    enhancement: Some(Enhancement::Stone),
                    ..Default::default()
                });
            }
            _ => {}
        }
    }
    fn on_hand_discarded(
        &self,
        cards: StackVec<&PlayingCard, MAX_PLAYED_HAND_CARDS>,
        hand: Hand,
        chips: &mut f32,
        mult: &mut f32,
        in_game: &mut InGame,
    ) {
        use JokerType::*;

        match self.joker_type {
            _ => {}
        }
    }
    fn on_hand_played(&self, hand: Hand, chips: &mut f32, mult: &mut f32, in_game: &mut InGame) {
        use JokerType::*;

        match self.joker_type {
            _ => {}
        }
    }
    fn on_card_scored(&self, card: &PlayingCard, hand: Hand, chips: &mut f32, mult: &mut f32) {
        use Hand::*;
        use JokerType::*;
        use PlayingCardSuit::*;

        macro_rules! suit_joker {
            ($($joker:ident - $suit:ident),*) => {
                match self.joker_type {
                    $(
                        $joker if card.suit == $suit => *mult += 3.,
                    )*
                    _ => {}
                };
            };
        }
        suit_joker!(
            GreedyJoker - Diamonds,
            LustyJoker - Hearts,
            WrathfulJoker - Spades,
            GluttonousJoker - Clubs
        );

        macro_rules! hand_joker {
            ($($joker:ident - $hand:ident - $(m $mult:literal)? $(c $chips:literal)?),*) => {
                match self.joker_type {
                    $(
                        $joker if hand == $hand => $(*mult += $mult)? $(*chips += $chips)?,
                    )*
                    _ => {}
                }
            };
        }
        hand_joker!(
            JollyJoker - Pair - m 8.,
            ZanyJoker - ThreeOfAKind - m 12.,
            MadJoker - TwoPair - m 10.,
            CrazyJoker - Straight - m 12.,
            DrollJoker - Flush - m 10.,
            SlyJoker - Pair - c 50.,
            WilyJoker - ThreeOfAKind - c 100.,
            CleverJoker - TwoPair - c 80.,
            DeviousJoker - Straight - c 100.,
            CraftyJoker - Flush - c 80.
        );

        match self.joker_type {
            // TODO: create tarot
            EightBall if card.rank == PlayingCardRank::Eight => {}
            _ => {}
        }
    }
    fn on_card_in_hand(&self, card: &PlayingCard, chips: &mut f32, mult: &mut f32) {
        match self.joker_type {
            _ => {}
        }
    }
    /// On the final triggering of all jokers, without an "event"
    fn on_triggered(
        &mut self,
        chips: &mut f32,
        mult: &mut f32,
        in_round: &mut InRound,
        played_cards: StackVec<&PlayingCard, MAX_PLAYED_HAND_CARDS>,
        rng: &mut Rng,
    ) {
        use JokerType::*;

        match &mut self.joker_type {
            Joker => *mult += 4.,
            HalfJoker if played_cards.len() <= 3 => *mult += 20.,
            JokerStencil => {
                *mult *= 1.
                    + (in_round
                        .in_game
                        .joker_slots
                        .saturating_sub(in_round.in_game.jokers.len() as u8)) // don't go negative for the mult
                        as f32
            }
            Banner => *chips += 30. * in_round.discards_remaining as f32,
            MysticSummit if in_round.discards_remaining == 0 => *mult += 15.,
            LoyaltyCard(remaining) => match remaining {
                0 => {
                    *mult *= 4.;
                    *remaining = 5
                }
                1..=5 => *remaining -= 1,
                _ => unreachable!(),
            },
            Misprint => *mult += rng.u8(..=23) as f32,
            // TODO: do once we move away from indices
            // RaisedFist => *mult += 2 * in_round.hand_card_indices,
            _ => {}
        }
    }
    fn on_round_exit(&self, in_game: &mut InGame) {
        use JokerType::*;

        match self.joker_type {
            _ => {}
        }
    }
    fn on_sold(&self, in_game: &mut InGame) {
        use JokerType::*;

        match self.joker_type {
            _ => {}
        }
    }
}

#[derive(Debug, Serialize)]
pub struct JokerEffectType {
    pub chips: bool,
    pub add_mult: bool,
    pub mult_mult: bool,
    pub effect: bool,
    pub retrigger: bool,
    pub economy: bool,
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

#[derive(Debug, Serialize)]
pub struct JokerCompatibility {
    pub copyable: bool,
    pub perishable: bool,
    pub eternal: bool,
}
impl JokerCompatibility {
    pub fn new(copyable: bool, perishable: bool, eternal: bool) -> Self {
        Self {
            copyable,
            perishable,
            eternal,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Edition {
    Foil,
    Holographic,
    Polychrome,
    Negative,
}
impl Price for Edition {
    fn buy_price(&self) -> u8 {
        match self {
            Edition::Foil => 2,
            Edition::Holographic => 3,
            Edition::Polychrome => 5,
            Edition::Negative => 5,
        }
    }
}

#[derive(Debug, Serialize)]
pub enum Rarity {
    Common,
    Uncommon,
    Rare,
    Legendary,
}

#[derive(Debug, Serialize)]
#[serialize(buy_price, sell_price)]
pub enum Consumable {
    Tarot(Tarot),
    Planet(Planet),
    Spectral(Spectral),
}
impl Price for Consumable {
    fn buy_price(&self) -> u8 {
        match self {
            Consumable::Tarot(tarot) => tarot.buy_price(),
            Consumable::Planet(planet) => planet.buy_price(),
            Consumable::Spectral(spectral) => spectral.buy_price(),
        }
    }
}

#[derive(Debug, Serialize, Default, Clone)]
pub struct PlayingCard {
    suit: PlayingCardSuit,
    rank: PlayingCardRank,
    edition: Option<Edition>,
    enhancement: Option<Enhancement>,
    seal: Option<Seal>,
    sticker: Option<Sticker>,
}
impl PlayingCard {
    fn hand(
        cards: &StackVec<PlayingCard, MAX_PLAYED_HAND_CARDS>,
    ) -> (Hand, [bool; MAX_PLAYED_HAND_CARDS]) {
        let (mut best_hand, mut included);

        let mut cards: StackVec<_, MAX_PLAYED_HAND_CARDS> = cards
            .iter()
            .filter(|card| card.enhancement != Some(Enhancement::Stone))
            .enumerate()
            .collect();
        cards.sort_unstable_by_key(|(_, card)| card.rank);

        // n-of-a-kinds
        let mut n_of_a_kinds = StackVec::<
            (PlayingCardRank, [bool; MAX_PLAYED_HAND_CARDS]),
            MAX_PLAYED_HAND_CARDS,
        >::new();

        let mut last_rank = None;
        for card in &cards.deref()[1..] {
            if Some(card.1.rank) == last_rank {
                let len = n_of_a_kinds.len();
                n_of_a_kinds[len].1[card.0] = true;
            } else {
                last_rank = Some(card.1.rank);
                n_of_a_kinds
                    .push((card.1.rank, array::from_fn(|i| i == card.0)))
                    .unwrap();
            }
        }
        let num_ones = |array: [bool; MAX_PLAYED_HAND_CARDS]| array.iter().filter(|i| **i).count();
        n_of_a_kinds.sort_unstable_by(|a, b| num_ones(a.1).cmp(&num_ones(b.1)).reverse());

        let combine = |a: [bool; _], b: [bool; _]| array::from_fn(|i| a[i] || b[i]);

        (best_hand, included) = match (
            num_ones(n_of_a_kinds[0].1),
            n_of_a_kinds.get(1).map(|e| num_ones(e.1)),
        ) {
            (5, _) => (Hand::FiveOfAKind, n_of_a_kinds[0].1),
            (4, _) => (Hand::FourOfAKind, n_of_a_kinds[0].1),
            (3, Some(2)) => (
                Hand::FullHouse,
                combine(n_of_a_kinds[0].1, n_of_a_kinds[1].1),
            ),
            (3, _) => (Hand::ThreeOfAKind, n_of_a_kinds[0].1),
            (2, Some(2)) => (Hand::TwoPair, combine(n_of_a_kinds[0].1, n_of_a_kinds[1].1)),
            (2, _) => (Hand::Pair, n_of_a_kinds[0].1),
            (1, _) => (Hand::HighCard, n_of_a_kinds[0].1),
            _ => unreachable!(),
        };

        let is_consecutive = |[a, b]: &[(_, &PlayingCard); 2]| a.1.rank as u8 + 1 == b.1.rank as u8;
        let is_straight = cards.array_windows().all(is_consecutive) // simple case
            // low ace
            || (cards.get(MAX_PLAYED_HAND_CARDS - 1).map(|card| card.1.rank)
                == Some(PlayingCardRank::Ace)
                &&
                cards.deref()[1..].array_windows()
                    .all(is_consecutive));
        if is_straight && Hand::Straight > best_hand {
            (best_hand, included) = (Hand::Straight, [true; MAX_PLAYED_HAND_CARDS]);
        }

        let is_flush = cards.array_windows().all(|[a, b]| a.1.suit == b.1.suit);

        if is_flush {
            best_hand = match best_hand {
                Hand::FiveOfAKind => Hand::FlushFive,
                Hand::FullHouse => Hand::FlushHouse,
                Hand::Straight => {
                    if cards[4].1.rank == PlayingCardRank::Ace {
                        Hand::RoyalFlush
                    } else {
                        Hand::StraightFlush
                    }
                }
                other if other >= Hand::Flush => {
                    included = [true; MAX_PLAYED_HAND_CARDS];
                    other
                }
                other => other,
            };
        }

        (best_hand, included)
    }
    fn on_score(&self, chips: &mut f32, mult: &mut f32) {
        // chips
        *chips += self.rank.chips() as f32;
        if self.edition == Some(Edition::Foil) {
            *chips += 50.;
        }
        *chips += match self.enhancement {
            Some(Enhancement::Bonus) => 30.,
            Some(Enhancement::Stone) => 50.,
            _ => 0.,
        };

        // +-mult
        if self.enhancement == Some(Enhancement::Mult) {
            *mult += 4.;
        }
        if self.edition == Some(Edition::Holographic) {
            *mult += 10.;
        }

        // x-mult
        if self.enhancement == Some(Enhancement::Glass) {
            *mult *= 2.;
        }
        if self.edition == Some(Edition::Polychrome) {
            *mult *= 1.5;
        }
    }
}
impl From<(PlayingCardSuit, PlayingCardRank)> for PlayingCard {
    fn from((suit, rank): (PlayingCardSuit, PlayingCardRank)) -> Self {
        Self {
            suit,
            rank,
            edition: None,
            enhancement: None,
            seal: None,
            sticker: None,
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Default, Clone, Copy)]
pub enum PlayingCardSuit {
    #[default]
    Hearts,
    Spades,
    Clubs,
    Diamonds,
}
impl PlayingCardSuit {
    fn random(rng: &mut Rng) -> Self {
        let num = rng.u8(0..4);
        match num {
            0 => Self::Hearts,
            1 => Self::Spades,
            2 => Self::Clubs,
            3 => Self::Diamonds,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Default)]
#[serialize(is_face, is_numbered, is_even, is_odd)]
#[repr(u8)]
pub enum PlayingCardRank {
    #[default]
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}
impl PlayingCardRank {
    const NUM_VARIANTS: u8 = 13;

    fn random(rng: &mut Rng) -> Self {
        let num = rng.u8(0..Self::NUM_VARIANTS);
        match num {
            0 => Self::Ace,
            1 => Self::Two,
            2 => Self::Three,
            3 => Self::Four,
            4 => Self::Five,
            5 => Self::Six,
            6 => Self::Seven,
            7 => Self::Eight,
            8 => Self::Nine,
            9 => Self::Ten,
            10 => Self::Jack,
            11 => Self::Queen,
            12 => Self::King,
            _ => unreachable!(),
        }
    }
    // TODO: consider pareidolia
    pub fn is_face(&self) -> bool {
        matches!(self, Self::Jack | Self::Queen | Self::King)
    }
    pub fn is_numbered(&self) -> bool {
        !self.is_face()
    }
    pub fn is_even(&self) -> bool {
        matches!(
            self,
            Self::Two | Self::Four | Self::Six | Self::Eight | Self::Ten
        )
    }
    pub fn is_odd(&self) -> bool {
        matches!(
            self,
            Self::Ace | Self::Three | Self::Five | Self::Seven | Self::Nine
        )
    }
    pub fn chips(&self) -> u8 {
        use PlayingCardRank::*;

        match self {
            Two => 2,
            Three => 3,
            Four => 4,
            Five => 5,
            Six => 6,
            Seven => 7,
            Eight => 8,
            Nine => 9,
            Ten | Jack | Queen | King => 10,
            Ace => 11,
        }
    }
}

#[repr(u8)]
#[derive(Debug, PartialEq, PartialOrd, Serialize, Clone, Copy)]
pub enum Hand {
    HighCard,
    Pair,
    TwoPair,
    ThreeOfAKind,
    Straight,
    Flush,
    FullHouse,
    FourOfAKind,
    StraightFlush,
    RoyalFlush,
    FiveOfAKind,
    FlushHouse,
    FlushFive,
}

/// [level; Hand]
#[derive(Debug, Serialize)]
struct HandLevels([u8; mem::variant_count::<Hand>()]);
impl HandLevels {
    // (chips, mult) of a given hand at the stored level
    fn chips_mult(&self, hand: Hand) -> (f32, f32) {
        use Hand::*;

        let level = self[hand] as f32;
        let (base_mult, base_chips, scale_mult, scale_chips) = match hand {
            HighCard => (1, 5, 1, 15),
            Pair => (2, 10, 1, 15),
            TwoPair => (2, 20, 1, 20),
            ThreeOfAKind => (3, 30, 2, 20),
            Straight => (4, 30, 3, 30),
            Flush => (4, 35, 2, 15),
            FullHouse => (4, 40, 2, 25),
            FourOfAKind => (7, 60, 3, 30),
            StraightFlush | RoyalFlush => (8, 100, 4, 40),
            FiveOfAKind => (12, 120, 3, 35),
            FlushHouse => (14, 140, 4, 40),
            FlushFive => (16, 160, 3, 50),
        };
        (
            base_mult as f32 + scale_mult as f32 * level,
            base_chips as f32 * scale_chips as f32 * level,
        )
    }
}
impl Index<Hand> for HandLevels {
    type Output = u8;
    fn index(&self, index: Hand) -> &Self::Output {
        &self.0[index as u8 as usize]
    }
}
impl IndexMut<Hand> for HandLevels {
    fn index_mut(&mut self, index: Hand) -> &mut Self::Output {
        &mut self.0[index as u8 as usize]
    }
}

#[derive(Debug, Serialize, Clone, Copy, PartialEq)]
pub enum Enhancement {
    Bonus,
    Mult,
    Wild,
    Glass,
    Steel,
    Stone,
    Gold,
    Lucky,
}

#[derive(Debug, Serialize, Clone, Copy)]
pub enum Seal {
    Gold,
    Red,
    Blue,
    Purple,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Sticker {
    Eternal,
    Perishable,
    Rental,
}

macro_rules! EnumWithNameImpl {
    ($enum:ident, [$($name:ident),+]) => {
        #[derive(Debug , Serialize)]
        pub enum $enum {
            $($name,)+
        }
        impl Name for $enum {
            fn name(&self) -> &'static str {
                match self {
                    $(Self::$name => concat!(stringify!($name), "", stringify!($enum))),+
                }
            }
        }
    };
}

EnumWithNameImpl!(
    Deck,
    [
        Red, Blue, Yellow, Green, Black, Magic, Nebula, Ghost, Abandoned, Checkered, Zodiac,
        Painted, Anaglyph, Plasma, Erratic
    ]
);
impl Deck {
    fn deck_cards(&self, rng: &mut Rng) -> DeckCards {
        let default = || {
            let mut default: DeckCards = StackVec::new();
            for i in 1..=4 {
                for j in 1..=13 {
                    let suit = match i {
                        1 => PlayingCardSuit::Hearts,
                        2 => PlayingCardSuit::Spades,
                        3 => PlayingCardSuit::Clubs,
                        4 => PlayingCardSuit::Diamonds,
                        _ => unreachable!(),
                    };
                    let rank = match j {
                        1 => PlayingCardRank::Ace,
                        2 => PlayingCardRank::Two,
                        3 => PlayingCardRank::Three,
                        4 => PlayingCardRank::Four,
                        5 => PlayingCardRank::Five,
                        6 => PlayingCardRank::Six,
                        7 => PlayingCardRank::Seven,
                        8 => PlayingCardRank::Eight,
                        9 => PlayingCardRank::Nine,
                        10 => PlayingCardRank::Ten,
                        11 => PlayingCardRank::Jack,
                        12 => PlayingCardRank::Queen,
                        13 => PlayingCardRank::King,
                        _ => unreachable!(),
                    };
                    default.push((suit, rank).into()).unwrap();
                }
            }
            default
        };
        match self {
            Self::Abandoned => {
                let mut deck = default();
                for i in 0..deck.len() {
                    if deck[i].rank.is_face() {
                        deck.remove(i).unwrap();
                    }
                }
                deck
            }
            Self::Checkered => {
                let mut deck = default();
                for i in 0..deck.len() {
                    if deck[i].suit == PlayingCardSuit::Clubs {
                        deck[i].suit = PlayingCardSuit::Spades
                    } else if deck[i].suit == PlayingCardSuit::Diamonds {
                        deck[i].suit = PlayingCardSuit::Hearts
                    }
                }
                deck
            }
            Self::Erratic => {
                let mut deck = StackVec::new();
                for _ in 0..deck.len() {
                    let suit = PlayingCardSuit::random(rng);
                    let rank = PlayingCardRank::random(rng);
                    deck.push((suit, rank).into()).unwrap();
                }
                deck
            }
            _ => default(),
        }
    }
}

EnumWithNameImpl!(Stake, [White, Red, Green, Black, Blue, Purple, Orane, Gold]);

#[derive(Debug, Serialize)]
pub enum BlindProgress {
    Small,
    Big,
    Boss,
}
impl BlindProgress {
    fn advance(&mut self, ante: &mut Ante) {
        *self = match self {
            Self::Small => Self::Big,
            Self::Big => Self::Boss,
            Self::Boss => {
                *ante += 1;
                Self::Small
            }
        };
    }
}

macro_rules! BossBlind {
    ($(($min_ante:literal $(, $name:ident  $(,($base_mult:literal))? $(,[$reward:literal])?)*)),+) => {
        #[derive(Debug, PartialEq , Serialize)]
        #[repr(u8)]
        pub enum BossBlind {
            $(
                $(
                    $name,
                )*
            )+
        }
        impl BossBlind {
            /// How many Variants are at the index ante
            const NUM_VARIANTS_AT_ANTE: [u8; 8] = [$(
                {let array: [&str;_] = [$(stringify!($name),)*]; array.len() as u8},
            )+];

            pub fn minimum_ante(&self) -> u8 {
                match &self {
                    $(
                        $(
                            Self::$name => $min_ante,
                        )*
                    )+
                }
            }
            pub fn base_score_multiplier(&self) -> u8 {
                match &self {
                    $(
                        $(
                            Self::$name =>  2 $(-2 + $base_mult)?,
                        )*
                    )+
                }
            }
            pub fn reward(&self) -> u8 {
                match &self {
                    $(
                        $(
                            Self::$name =>  5 $(-5 + $reward)?,
                        )*
                    )+
                }
            }
        }
    };
}
impl BossBlind {
    pub fn random(rng: &mut Rng, ante: Ante) -> Self {
        let ante = ante.min(8);
        let end = Self::NUM_VARIANTS_AT_ANTE[0..ante as usize].iter().sum();

        let u8 = rng.u8(0..end);
        // SAFETY:
        // - BossBlind does not carry any safety invariants, so Assume::SAFETY is justified.
        // - All values in the range specified above are valid `BossBlind`s, so Assume::VALIDITY is justified.
        // - All other safety invariants are guaranteed by the compiler.
        unsafe { TransmuteFrom::<_, { Assume::SAFETY.and(Assume::VALIDITY) }>::transmute(u8) }
    }
}
BossBlind!(
    (
        1, TheHook, TheClub, ThePsychic, TheGoad, TheWindow, TheManacle, ThePillar, TheHead
    ),
    (
        2, TheHouse, TheWall, TheWheel, TheArm, TheFish, TheWater, TheMouth, TheNeedle, TheFlint,
        TheMark
    ),
    (3, TheEye, TheTooth),
    (4, ThePlant),
    (5, TheSerpent, (1)),
    (6, TheOx, (4)),
    (7),
    (
        8,
        AmberAcorn,
        [8],
        VerdantLeaft,
        [8],
        VioletVessel,
        (6),
        [8],
        CrimsonHeart,
        [8],
        CeruleanBell,
        [8]
    )
);

#[derive(Debug, Clone, Copy, Serialize)]
pub enum Tag {
    Uncommon,
    Rare,
    Negative,
    Foil,
    Holographic,
    Polychrome,
    Investment,
    Voucher,
    Boss,
    Standard,
    Char,
    Meteor,
    Buffoon,
    Handy,
    Garbage,
    Ethereal,
    Coupon,
    Double,
    Juggle,
    D6,
    TopUp,
    Speed,
    Orbital,
    Economy,
}

macro_rules! Voucher {
    ($(($base:ident$(($ty:ident))?, $upgrade:ident)),+) => {
        #[derive(Debug, PartialEq, Serialize)]
        #[serialize(buy_price, sell_price)]
        pub enum Voucher {
            $(
                $base$(($ty))?,
                $upgrade,
            )+
        }
        impl Voucher {
            /// Returns the upgraded version of the voucher, if there is one
            #[allow(unused_variables)]
            pub fn upgrade(&self) -> Option<Self> {
                match self {
                    $(
                        Self::$base$(($ty))? => Some(Self::$upgrade),
                        Self::$upgrade => None,
                    )+
                }
            }
        }
    };
}
Voucher!(
    (Overstock, OverstockPlus),
    (ClearanceSale, Liquidation),
    (Hone, GlowUp),
    (RerollSurpluss, RerollGlut),
    (CrystalBall, OmenGlobe),
    (Telescope, Observatory),
    (Gravver, NachoTong),
    (Wasteful, Recyclomancy),
    (TarotMerchant, TarotTycoon),
    (PlanetMerchant, PlanetTycoon),
    (SeedMoney, MoneyTree),
    (Blank, Antimatter),
    (MagicTrick, Illusion),
    (Hieroglyph, Petroglyph),
    (DirectorsCut(bool), Retcon),
    (PaintBrush, Palette)
);
impl Price for Voucher {
    fn buy_price(&self) -> u8 {
        10
    }
}

// see codegen crate
// CODEGEN START
#[derive(Debug, Serialize)]
#[serialize(buy_price, sell_price)]
pub enum Tarot {
    TheFool,
    TheMagician,
    TheHighPriestess,
    TheEmpress,
    TheEmperor,
    TheHierophant,
    TheLovers,
    TheChariot,
    Justice,
    TheHermit,
    TheWheelofFortune,
    Strength,
    TheHangedMan,
    Death,
    Temperance,
    TheDevil,
    TheTower,
    TheStar,
    TheMoon,
    TheSun,
    Judgement,
    TheWorld,
}
impl Name for Tarot {
    fn name(&self) -> &'static str {
        match self {
            Self::TheFool => "The Fool",
            Self::TheMagician => "The Magician",
            Self::TheHighPriestess => "The High Priestess",
            Self::TheEmpress => "The Empress",
            Self::TheEmperor => "The Emperor",
            Self::TheHierophant => "The Hierophant",
            Self::TheLovers => "The Lovers",
            Self::TheChariot => "The Chariot",
            Self::Justice => "Justice",
            Self::TheHermit => "The Hermit",
            Self::TheWheelofFortune => "The Wheel of Fortune",
            Self::Strength => "Strength",
            Self::TheHangedMan => "The Hanged Man",
            Self::Death => "Death",
            Self::Temperance => "Temperance",
            Self::TheDevil => "The Devil",
            Self::TheTower => "The Tower",
            Self::TheStar => "The Star",
            Self::TheMoon => "The Moon",
            Self::TheSun => "The Sun",
            Self::Judgement => "Judgement",
            Self::TheWorld => "The World",
        }
    }
}
impl Price for Tarot {
    fn buy_price(&self) -> u8 {
        match self {
            Self::TheFool => 3,
            Self::TheMagician => 3,
            Self::TheHighPriestess => 3,
            Self::TheEmpress => 3,
            Self::TheEmperor => 3,
            Self::TheHierophant => 3,
            Self::TheLovers => 3,
            Self::TheChariot => 3,
            Self::Justice => 3,
            Self::TheHermit => 3,
            Self::TheWheelofFortune => 3,
            Self::Strength => 3,
            Self::TheHangedMan => 3,
            Self::Death => 3,
            Self::Temperance => 3,
            Self::TheDevil => 3,
            Self::TheTower => 3,
            Self::TheStar => 3,
            Self::TheMoon => 3,
            Self::TheSun => 3,
            Self::Judgement => 3,
            Self::TheWorld => 3,
        }
    }
}

#[derive(Debug, Serialize)]
#[serialize(buy_price, sell_price)]
pub enum Planet {
    Pluto,
    Mercury,
    Uranus,
    Venus,
    Saturn,
    Jupiter,
    Earth,
    Mars,
    Neptune,
    PlanetX,
    Ceres,
    Eris,
}
impl Name for Planet {
    fn name(&self) -> &'static str {
        match self {
            Self::Pluto => "Pluto",
            Self::Mercury => "Mercury",
            Self::Uranus => "Uranus",
            Self::Venus => "Venus",
            Self::Saturn => "Saturn",
            Self::Jupiter => "Jupiter",
            Self::Earth => "Earth",
            Self::Mars => "Mars",
            Self::Neptune => "Neptune",
            Self::PlanetX => "Planet X",
            Self::Ceres => "Ceres",
            Self::Eris => "Eris",
        }
    }
}
impl Price for Planet {
    fn buy_price(&self) -> u8 {
        match self {
            Self::Pluto => 3,
            Self::Mercury => 3,
            Self::Uranus => 3,
            Self::Venus => 3,
            Self::Saturn => 3,
            Self::Jupiter => 3,
            Self::Earth => 3,
            Self::Mars => 3,
            Self::Neptune => 3,
            Self::PlanetX => 3,
            Self::Ceres => 3,
            Self::Eris => 3,
        }
    }
}

#[derive(Debug, Serialize)]
#[serialize(buy_price, sell_price)]
pub enum Spectral {
    Familiar,
    Grim,
    Incantation,
    Talisman,
    Aura,
    Wraith,
    Sigil,
    Ouija,
    Ectoplasm,
    Immolate,
    Ankh,
    DejaVu,
    Hex,
    Trance,
    Medium,
    Cryptid,
    TheSoul,
    BlackHole,
}
impl Name for Spectral {
    fn name(&self) -> &'static str {
        match self {
            Self::Familiar => "Familiar",
            Self::Grim => "Grim",
            Self::Incantation => "Incantation",
            Self::Talisman => "Talisman",
            Self::Aura => "Aura",
            Self::Wraith => "Wraith",
            Self::Sigil => "Sigil",
            Self::Ouija => "Ouija",
            Self::Ectoplasm => "Ectoplasm",
            Self::Immolate => "Immolate",
            Self::Ankh => "Ankh",
            Self::DejaVu => "Deja Vu",
            Self::Hex => "Hex",
            Self::Trance => "Trance",
            Self::Medium => "Medium",
            Self::Cryptid => "Cryptid",
            Self::TheSoul => "The Soul",
            Self::BlackHole => "Black Hole",
        }
    }
}
impl Price for Spectral {
    fn buy_price(&self) -> u8 {
        match self {
            Self::Familiar => 4,
            Self::Grim => 4,
            Self::Incantation => 4,
            Self::Talisman => 4,
            Self::Aura => 4,
            Self::Wraith => 4,
            Self::Sigil => 4,
            Self::Ouija => 4,
            Self::Ectoplasm => 4,
            Self::Immolate => 4,
            Self::Ankh => 4,
            Self::DejaVu => 4,
            Self::Hex => 4,
            Self::Trance => 4,
            Self::Medium => 4,
            Self::Cryptid => 4,
            Self::TheSoul => 4,
            Self::BlackHole => 4,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serialize(rarity, effect_type, compatibility)]
pub enum JokerType {
    EightBall,
    AbstractJoker,
    Acrobat,
    AncientJoker,
    Arrowhead,
    Astronomer,
    Banner,
    Baron,
    BaseballCard,
    Blackboard,
    Bloodstone,
    BlueJoker,
    Blueprint,
    Bootstraps,
    Brainstorm,
    Bull,
    Burglar,
    BurntJoker,
    BusinessCard,
    Campfire,
    Canio,
    CardSharp,
    Cartomancer,
    Castle,
    Cavendish,
    /// (mult)
    CeremonialDagger(u16),
    Certificate,
    ChaostheClown,
    Chicot,
    CleverJoker,
    Cloud9,
    Constellation,
    CraftyJoker,
    CrazyJoker,
    CreditCard,
    DelayedGratification,
    DeviousJoker,
    DietCola,
    DNA,
    DriversLicense,
    DrollJoker,
    Drunkard,
    Dusk,
    Egg,
    Erosion,
    EvenSteven,
    FacelessJoker,
    Fibonacci,
    FlashCard,
    FlowerPot,
    FortuneTeller,
    FourFingers,
    GiftCard,
    GlassJoker,
    GluttonousJoker,
    GoldenJoker,
    GoldenTicket,
    GreedyJoker,
    GreenJoker,
    GrosMichel,
    Hack,
    HalfJoker,
    Hallucination,
    HangingChad,
    Hiker,
    HittheRoad,
    Hologram,
    IceCream,
    InvisibleJoker,
    Joker,
    JokerStencil,
    JollyJoker,
    Juggler,
    LoyaltyCard(u8),
    Luchador,
    LuckyCat,
    LustyJoker,
    MadJoker,
    Madness,
    MailInRebate,
    MarbleJoker,
    Matador,
    MerryAndy,
    MidasMask,
    Mime,
    Misprint,
    MrBones,
    MysticSummit,
    Obelisk,
    OddTodd,
    OnyxAgate,
    OopsAll6s,
    Pareidolia,
    Perkeo,
    Photograph,
    Popcorn,
    RaisedFist,
    Ramen,
    RedCard,
    ReservedParking,
    RidetheBus,
    RiffRaff,
    Rocket,
    RoughGem,
    Runner,
    Satellite,
    ScaryFace,
    Scholar,
    SeeingDouble,
    Seltzer,
    ShoottheMoon,
    Shortcut,
    Showman,
    SixthSense,
    SlyJoker,
    SmearedJoker,
    SmileyFace,
    SockandBuskin,
    SpaceJoker,
    SpareTrousers,
    Splash,
    SquareJoker,
    SteelJoker,
    StoneJoker,
    Stuntman,
    Supernova,
    Superposition,
    Swashbuckler,
    Séance,
    TheDuo,
    TheFamily,
    TheIdol,
    TheOrder,
    TheTribe,
    TheTrio,
    Throwback,
    ToDoList,
    TotheMoon,
    TradingCard,
    Triboulet,
    Troubadour,
    TurtleBean,
    Vagabond,
    Vampire,
    WalkieTalkie,
    WeeJoker,
    WilyJoker,
    WrathfulJoker,
    Yorick,
    ZanyJoker,
}
impl JokerType {
    pub fn rarity(&self) -> Rarity {
        match self {
            Self::EightBall => Rarity::Common,
            Self::AbstractJoker => Rarity::Common,
            Self::Acrobat => Rarity::Uncommon,
            Self::AncientJoker => Rarity::Rare,
            Self::Arrowhead => Rarity::Uncommon,
            Self::Astronomer => Rarity::Uncommon,
            Self::Banner => Rarity::Common,
            Self::Baron => Rarity::Rare,
            Self::BaseballCard => Rarity::Rare,
            Self::Blackboard => Rarity::Uncommon,
            Self::Bloodstone => Rarity::Uncommon,
            Self::BlueJoker => Rarity::Common,
            Self::Blueprint => Rarity::Rare,
            Self::Bootstraps => Rarity::Uncommon,
            Self::Brainstorm => Rarity::Rare,
            Self::Bull => Rarity::Uncommon,
            Self::Burglar => Rarity::Uncommon,
            Self::BurntJoker => Rarity::Rare,
            Self::BusinessCard => Rarity::Common,
            Self::Campfire => Rarity::Rare,
            Self::Canio => Rarity::Legendary,
            Self::CardSharp => Rarity::Uncommon,
            Self::Cartomancer => Rarity::Uncommon,
            Self::Castle => Rarity::Uncommon,
            Self::Cavendish => Rarity::Common,
            Self::CeremonialDagger(_) => Rarity::Uncommon,
            Self::Certificate => Rarity::Uncommon,
            Self::ChaostheClown => Rarity::Common,
            Self::Chicot => Rarity::Legendary,
            Self::CleverJoker => Rarity::Common,
            Self::Cloud9 => Rarity::Uncommon,
            Self::Constellation => Rarity::Uncommon,
            Self::CraftyJoker => Rarity::Common,
            Self::CrazyJoker => Rarity::Common,
            Self::CreditCard => Rarity::Common,
            Self::DelayedGratification => Rarity::Common,
            Self::DeviousJoker => Rarity::Common,
            Self::DietCola => Rarity::Uncommon,
            Self::DNA => Rarity::Rare,
            Self::DriversLicense => Rarity::Rare,
            Self::DrollJoker => Rarity::Common,
            Self::Drunkard => Rarity::Common,
            Self::Dusk => Rarity::Uncommon,
            Self::Egg => Rarity::Common,
            Self::Erosion => Rarity::Uncommon,
            Self::EvenSteven => Rarity::Common,
            Self::FacelessJoker => Rarity::Common,
            Self::Fibonacci => Rarity::Uncommon,
            Self::FlashCard => Rarity::Uncommon,
            Self::FlowerPot => Rarity::Uncommon,
            Self::FortuneTeller => Rarity::Common,
            Self::FourFingers => Rarity::Uncommon,
            Self::GiftCard => Rarity::Uncommon,
            Self::GlassJoker => Rarity::Uncommon,
            Self::GluttonousJoker => Rarity::Common,
            Self::GoldenJoker => Rarity::Common,
            Self::GoldenTicket => Rarity::Common,
            Self::GreedyJoker => Rarity::Common,
            Self::GreenJoker => Rarity::Common,
            Self::GrosMichel => Rarity::Common,
            Self::Hack => Rarity::Uncommon,
            Self::HalfJoker => Rarity::Common,
            Self::Hallucination => Rarity::Common,
            Self::HangingChad => Rarity::Common,
            Self::Hiker => Rarity::Uncommon,
            Self::HittheRoad => Rarity::Rare,
            Self::Hologram => Rarity::Uncommon,
            Self::IceCream => Rarity::Common,
            Self::InvisibleJoker => Rarity::Rare,
            Self::Joker => Rarity::Common,
            Self::JokerStencil => Rarity::Uncommon,
            Self::JollyJoker => Rarity::Common,
            Self::Juggler => Rarity::Common,
            Self::LoyaltyCard(_) => Rarity::Uncommon,
            Self::Luchador => Rarity::Uncommon,
            Self::LuckyCat => Rarity::Uncommon,
            Self::LustyJoker => Rarity::Common,
            Self::MadJoker => Rarity::Common,
            Self::Madness => Rarity::Uncommon,
            Self::MailInRebate => Rarity::Common,
            Self::MarbleJoker => Rarity::Uncommon,
            Self::Matador => Rarity::Uncommon,
            Self::MerryAndy => Rarity::Uncommon,
            Self::MidasMask => Rarity::Uncommon,
            Self::Mime => Rarity::Uncommon,
            Self::Misprint => Rarity::Common,
            Self::MrBones => Rarity::Uncommon,
            Self::MysticSummit => Rarity::Common,
            Self::Obelisk => Rarity::Rare,
            Self::OddTodd => Rarity::Common,
            Self::OnyxAgate => Rarity::Uncommon,
            Self::OopsAll6s => Rarity::Uncommon,
            Self::Pareidolia => Rarity::Uncommon,
            Self::Perkeo => Rarity::Legendary,
            Self::Photograph => Rarity::Common,
            Self::Popcorn => Rarity::Common,
            Self::RaisedFist => Rarity::Common,
            Self::Ramen => Rarity::Uncommon,
            Self::RedCard => Rarity::Common,
            Self::ReservedParking => Rarity::Common,
            Self::RidetheBus => Rarity::Common,
            Self::RiffRaff => Rarity::Common,
            Self::Rocket => Rarity::Uncommon,
            Self::RoughGem => Rarity::Uncommon,
            Self::Runner => Rarity::Common,
            Self::Satellite => Rarity::Uncommon,
            Self::ScaryFace => Rarity::Common,
            Self::Scholar => Rarity::Common,
            Self::SeeingDouble => Rarity::Uncommon,
            Self::Seltzer => Rarity::Uncommon,
            Self::ShoottheMoon => Rarity::Common,
            Self::Shortcut => Rarity::Uncommon,
            Self::Showman => Rarity::Uncommon,
            Self::SixthSense => Rarity::Uncommon,
            Self::SlyJoker => Rarity::Common,
            Self::SmearedJoker => Rarity::Uncommon,
            Self::SmileyFace => Rarity::Common,
            Self::SockandBuskin => Rarity::Uncommon,
            Self::SpaceJoker => Rarity::Uncommon,
            Self::SpareTrousers => Rarity::Uncommon,
            Self::Splash => Rarity::Common,
            Self::SquareJoker => Rarity::Common,
            Self::SteelJoker => Rarity::Uncommon,
            Self::StoneJoker => Rarity::Uncommon,
            Self::Stuntman => Rarity::Rare,
            Self::Supernova => Rarity::Common,
            Self::Superposition => Rarity::Common,
            Self::Swashbuckler => Rarity::Common,
            Self::Séance => Rarity::Uncommon,
            Self::TheDuo => Rarity::Rare,
            Self::TheFamily => Rarity::Rare,
            Self::TheIdol => Rarity::Uncommon,
            Self::TheOrder => Rarity::Rare,
            Self::TheTribe => Rarity::Rare,
            Self::TheTrio => Rarity::Rare,
            Self::Throwback => Rarity::Uncommon,
            Self::ToDoList => Rarity::Common,
            Self::TotheMoon => Rarity::Uncommon,
            Self::TradingCard => Rarity::Uncommon,
            Self::Triboulet => Rarity::Legendary,
            Self::Troubadour => Rarity::Uncommon,
            Self::TurtleBean => Rarity::Uncommon,
            Self::Vagabond => Rarity::Rare,
            Self::Vampire => Rarity::Uncommon,
            Self::WalkieTalkie => Rarity::Common,
            Self::WeeJoker => Rarity::Rare,
            Self::WilyJoker => Rarity::Common,
            Self::WrathfulJoker => Rarity::Common,
            Self::Yorick => Rarity::Legendary,
            Self::ZanyJoker => Rarity::Common,
        }
    }
    pub fn effect_type(&self) -> JokerEffectType {
        match self {
            Self::EightBall => JokerEffectType::new(false, false, false, true, false, false),
            Self::AbstractJoker => JokerEffectType::new(false, true, false, false, false, false),
            Self::Acrobat => JokerEffectType::new(false, false, true, false, false, false),
            Self::AncientJoker => JokerEffectType::new(false, false, true, false, false, false),
            Self::Arrowhead => JokerEffectType::new(true, false, false, false, false, false),
            Self::Astronomer => JokerEffectType::new(false, false, false, true, false, false),
            Self::Banner => JokerEffectType::new(true, false, false, false, false, false),
            Self::Baron => JokerEffectType::new(false, false, true, false, false, false),
            Self::BaseballCard => JokerEffectType::new(false, false, true, false, false, false),
            Self::Blackboard => JokerEffectType::new(false, false, true, false, false, false),
            Self::Bloodstone => JokerEffectType::new(false, false, true, false, false, false),
            Self::BlueJoker => JokerEffectType::new(true, false, false, false, false, false),
            Self::Blueprint => JokerEffectType::new(false, false, false, true, false, false),
            Self::Bootstraps => JokerEffectType::new(false, true, false, false, false, false),
            Self::Brainstorm => JokerEffectType::new(false, false, false, true, false, false),
            Self::Bull => JokerEffectType::new(true, false, false, false, false, false),
            Self::Burglar => JokerEffectType::new(false, false, false, true, false, false),
            Self::BurntJoker => JokerEffectType::new(false, false, false, true, false, false),
            Self::BusinessCard => JokerEffectType::new(false, false, false, false, false, true),
            Self::Campfire => JokerEffectType::new(false, false, true, false, false, false),
            Self::Canio => JokerEffectType::new(false, false, true, false, false, false),
            Self::CardSharp => JokerEffectType::new(false, false, true, false, false, false),
            Self::Cartomancer => JokerEffectType::new(false, false, false, true, false, false),
            Self::Castle => JokerEffectType::new(true, false, false, false, false, false),
            Self::Cavendish => JokerEffectType::new(false, false, true, false, false, false),
            Self::CeremonialDagger(_) => {
                JokerEffectType::new(false, true, false, false, false, false)
            }
            Self::Certificate => JokerEffectType::new(false, false, false, true, false, false),
            Self::ChaostheClown => JokerEffectType::new(false, false, false, true, false, false),
            Self::Chicot => JokerEffectType::new(false, false, false, true, false, false),
            Self::CleverJoker => JokerEffectType::new(true, false, false, false, false, false),
            Self::Cloud9 => JokerEffectType::new(false, false, false, false, false, true),
            Self::Constellation => JokerEffectType::new(false, false, true, false, false, false),
            Self::CraftyJoker => JokerEffectType::new(true, false, false, false, false, false),
            Self::CrazyJoker => JokerEffectType::new(false, true, false, false, false, false),
            Self::CreditCard => JokerEffectType::new(false, false, false, false, false, true),
            Self::DelayedGratification => {
                JokerEffectType::new(false, false, false, false, false, true)
            }
            Self::DeviousJoker => JokerEffectType::new(true, false, false, false, false, false),
            Self::DietCola => JokerEffectType::new(false, false, false, true, false, false),
            Self::DNA => JokerEffectType::new(false, false, false, true, false, false),
            Self::DriversLicense => JokerEffectType::new(false, false, true, false, false, false),
            Self::DrollJoker => JokerEffectType::new(false, true, false, false, false, false),
            Self::Drunkard => JokerEffectType::new(false, false, false, true, false, false),
            Self::Dusk => JokerEffectType::new(false, false, false, false, true, false),
            Self::Egg => JokerEffectType::new(false, false, false, false, false, true),
            Self::Erosion => JokerEffectType::new(false, true, false, false, false, false),
            Self::EvenSteven => JokerEffectType::new(false, true, false, false, false, false),
            Self::FacelessJoker => JokerEffectType::new(false, false, false, false, false, true),
            Self::Fibonacci => JokerEffectType::new(false, true, false, false, false, false),
            Self::FlashCard => JokerEffectType::new(false, true, false, false, false, false),
            Self::FlowerPot => JokerEffectType::new(false, false, true, false, false, false),
            Self::FortuneTeller => JokerEffectType::new(false, true, false, false, false, false),
            Self::FourFingers => JokerEffectType::new(false, false, false, true, false, false),
            Self::GiftCard => JokerEffectType::new(false, false, false, false, false, true),
            Self::GlassJoker => JokerEffectType::new(false, false, true, false, false, false),
            Self::GluttonousJoker => JokerEffectType::new(false, true, false, false, false, false),
            Self::GoldenJoker => JokerEffectType::new(false, false, false, false, false, true),
            Self::GoldenTicket => JokerEffectType::new(false, false, false, false, false, true),
            Self::GreedyJoker => JokerEffectType::new(false, true, false, false, false, false),
            Self::GreenJoker => JokerEffectType::new(false, true, false, false, false, false),
            Self::GrosMichel => JokerEffectType::new(false, true, false, false, false, false),
            Self::Hack => JokerEffectType::new(false, false, false, false, true, false),
            Self::HalfJoker => JokerEffectType::new(false, true, false, false, false, false),
            Self::Hallucination => JokerEffectType::new(false, false, false, true, false, false),
            Self::HangingChad => JokerEffectType::new(false, false, false, false, true, false),
            Self::Hiker => JokerEffectType::new(true, false, false, false, false, false),
            Self::HittheRoad => JokerEffectType::new(false, false, true, false, false, false),
            Self::Hologram => JokerEffectType::new(false, false, true, false, false, false),
            Self::IceCream => JokerEffectType::new(true, false, false, false, false, false),
            Self::InvisibleJoker => JokerEffectType::new(false, false, false, true, false, false),
            Self::Joker => JokerEffectType::new(false, true, false, false, false, false),
            Self::JokerStencil => JokerEffectType::new(false, false, true, false, false, false),
            Self::JollyJoker => JokerEffectType::new(false, true, false, false, false, false),
            Self::Juggler => JokerEffectType::new(false, false, false, true, false, false),
            Self::LoyaltyCard(_) => JokerEffectType::new(false, false, true, false, false, false),
            Self::Luchador => JokerEffectType::new(false, false, false, true, false, false),
            Self::LuckyCat => JokerEffectType::new(false, false, true, false, false, false),
            Self::LustyJoker => JokerEffectType::new(false, true, false, false, false, false),
            Self::MadJoker => JokerEffectType::new(false, true, false, false, false, false),
            Self::Madness => JokerEffectType::new(false, false, true, false, false, false),
            Self::MailInRebate => JokerEffectType::new(false, false, false, false, false, true),
            Self::MarbleJoker => JokerEffectType::new(false, false, false, true, false, false),
            Self::Matador => JokerEffectType::new(false, false, false, false, false, true),
            Self::MerryAndy => JokerEffectType::new(false, false, false, true, false, false),
            Self::MidasMask => JokerEffectType::new(false, false, false, true, false, false),
            Self::Mime => JokerEffectType::new(false, false, false, false, true, false),
            Self::Misprint => JokerEffectType::new(false, true, false, false, false, false),
            Self::MrBones => JokerEffectType::new(false, false, false, true, false, false),
            Self::MysticSummit => JokerEffectType::new(false, true, false, false, false, false),
            Self::Obelisk => JokerEffectType::new(false, false, true, false, false, false),
            Self::OddTodd => JokerEffectType::new(true, false, false, false, false, false),
            Self::OnyxAgate => JokerEffectType::new(false, true, false, false, false, false),
            Self::OopsAll6s => JokerEffectType::new(false, false, false, true, false, false),
            Self::Pareidolia => JokerEffectType::new(false, false, false, true, false, false),
            Self::Perkeo => JokerEffectType::new(false, false, false, true, false, false),
            Self::Photograph => JokerEffectType::new(false, false, true, false, false, false),
            Self::Popcorn => JokerEffectType::new(false, true, false, false, false, false),
            Self::RaisedFist => JokerEffectType::new(false, true, false, false, false, false),
            Self::Ramen => JokerEffectType::new(false, false, true, false, false, false),
            Self::RedCard => JokerEffectType::new(false, true, false, false, false, false),
            Self::ReservedParking => JokerEffectType::new(false, false, false, false, false, true),
            Self::RidetheBus => JokerEffectType::new(false, true, false, false, false, false),
            Self::RiffRaff => JokerEffectType::new(false, false, false, true, false, false),
            Self::Rocket => JokerEffectType::new(false, false, false, false, false, true),
            Self::RoughGem => JokerEffectType::new(false, false, false, false, false, true),
            Self::Runner => JokerEffectType::new(true, false, false, false, false, false),
            Self::Satellite => JokerEffectType::new(false, false, false, false, false, true),
            Self::ScaryFace => JokerEffectType::new(true, false, false, false, false, false),
            Self::Scholar => JokerEffectType::new(false, true, false, false, false, false),
            Self::SeeingDouble => JokerEffectType::new(false, false, true, false, false, false),
            Self::Seltzer => JokerEffectType::new(false, false, false, false, true, false),
            Self::ShoottheMoon => JokerEffectType::new(false, true, false, false, false, false),
            Self::Shortcut => JokerEffectType::new(false, false, false, true, false, false),
            Self::Showman => JokerEffectType::new(false, false, false, true, false, false),
            Self::SixthSense => JokerEffectType::new(false, false, false, true, false, false),
            Self::SlyJoker => JokerEffectType::new(true, false, false, false, false, false),
            Self::SmearedJoker => JokerEffectType::new(false, false, false, true, false, false),
            Self::SmileyFace => JokerEffectType::new(false, true, false, false, false, false),
            Self::SockandBuskin => JokerEffectType::new(false, false, false, false, true, false),
            Self::SpaceJoker => JokerEffectType::new(false, false, false, true, false, false),
            Self::SpareTrousers => JokerEffectType::new(false, true, false, false, false, false),
            Self::Splash => JokerEffectType::new(false, false, false, true, false, false),
            Self::SquareJoker => JokerEffectType::new(true, false, false, false, false, false),
            Self::SteelJoker => JokerEffectType::new(false, false, true, false, false, false),
            Self::StoneJoker => JokerEffectType::new(true, false, false, false, false, false),
            Self::Stuntman => JokerEffectType::new(true, false, false, false, false, false),
            Self::Supernova => JokerEffectType::new(false, true, false, false, false, false),
            Self::Superposition => JokerEffectType::new(false, false, false, true, false, false),
            Self::Swashbuckler => JokerEffectType::new(false, true, false, false, false, false),
            Self::Séance => JokerEffectType::new(false, false, false, true, false, false),
            Self::TheDuo => JokerEffectType::new(false, false, true, false, false, false),
            Self::TheFamily => JokerEffectType::new(false, false, true, false, false, false),
            Self::TheIdol => JokerEffectType::new(false, false, true, false, false, false),
            Self::TheOrder => JokerEffectType::new(false, false, true, false, false, false),
            Self::TheTribe => JokerEffectType::new(false, false, true, false, false, false),
            Self::TheTrio => JokerEffectType::new(false, false, true, false, false, false),
            Self::Throwback => JokerEffectType::new(false, false, true, false, false, false),
            Self::ToDoList => JokerEffectType::new(false, false, false, false, false, true),
            Self::TotheMoon => JokerEffectType::new(false, false, false, false, false, true),
            Self::TradingCard => JokerEffectType::new(false, false, false, false, false, true),
            Self::Triboulet => JokerEffectType::new(false, false, true, false, false, false),
            Self::Troubadour => JokerEffectType::new(false, false, false, true, false, false),
            Self::TurtleBean => JokerEffectType::new(false, false, false, true, false, false),
            Self::Vagabond => JokerEffectType::new(false, false, false, true, false, false),
            Self::Vampire => JokerEffectType::new(false, false, true, false, false, false),
            Self::WalkieTalkie => JokerEffectType::new(false, true, false, false, false, false),
            Self::WeeJoker => JokerEffectType::new(true, false, false, false, false, false),
            Self::WilyJoker => JokerEffectType::new(true, false, false, false, false, false),
            Self::WrathfulJoker => JokerEffectType::new(false, true, false, false, false, false),
            Self::Yorick => JokerEffectType::new(false, false, true, false, false, false),
            Self::ZanyJoker => JokerEffectType::new(false, true, false, false, false, false),
        }
    }
    pub fn compatibility(&self) -> JokerCompatibility {
        match self {
            Self::EightBall => JokerCompatibility::new(true, true, true),
            Self::AbstractJoker => JokerCompatibility::new(true, true, true),
            Self::Acrobat => JokerCompatibility::new(true, true, true),
            Self::AncientJoker => JokerCompatibility::new(true, true, true),
            Self::Arrowhead => JokerCompatibility::new(true, true, true),
            Self::Astronomer => JokerCompatibility::new(false, true, true),
            Self::Banner => JokerCompatibility::new(true, true, true),
            Self::Baron => JokerCompatibility::new(true, true, true),
            Self::BaseballCard => JokerCompatibility::new(true, true, true),
            Self::Blackboard => JokerCompatibility::new(true, true, true),
            Self::Bloodstone => JokerCompatibility::new(true, true, true),
            Self::BlueJoker => JokerCompatibility::new(true, true, true),
            Self::Blueprint => JokerCompatibility::new(true, true, true),
            Self::Bootstraps => JokerCompatibility::new(true, true, true),
            Self::Brainstorm => JokerCompatibility::new(true, true, true),
            Self::Bull => JokerCompatibility::new(true, true, true),
            Self::Burglar => JokerCompatibility::new(true, true, true),
            Self::BurntJoker => JokerCompatibility::new(true, true, true),
            Self::BusinessCard => JokerCompatibility::new(true, true, true),
            Self::Campfire => JokerCompatibility::new(true, true, true),
            Self::Canio => JokerCompatibility::new(true, true, true),
            Self::CardSharp => JokerCompatibility::new(true, true, true),
            Self::Cartomancer => JokerCompatibility::new(true, true, true),
            Self::Castle => JokerCompatibility::new(true, false, true),
            Self::Cavendish => JokerCompatibility::new(true, true, false),
            Self::CeremonialDagger(_) => JokerCompatibility::new(true, false, true),
            Self::Certificate => JokerCompatibility::new(true, true, true),
            Self::ChaostheClown => JokerCompatibility::new(false, true, true),
            Self::Chicot => JokerCompatibility::new(false, true, true),
            Self::CleverJoker => JokerCompatibility::new(true, true, true),
            Self::Cloud9 => JokerCompatibility::new(false, true, true),
            Self::Constellation => JokerCompatibility::new(true, false, true),
            Self::CraftyJoker => JokerCompatibility::new(true, true, true),
            Self::CrazyJoker => JokerCompatibility::new(true, true, true),
            Self::CreditCard => JokerCompatibility::new(false, true, true),
            Self::DelayedGratification => JokerCompatibility::new(false, true, true),
            Self::DeviousJoker => JokerCompatibility::new(true, true, true),
            Self::DietCola => JokerCompatibility::new(true, true, false),
            Self::DNA => JokerCompatibility::new(true, true, true),
            Self::DriversLicense => JokerCompatibility::new(true, true, true),
            Self::DrollJoker => JokerCompatibility::new(true, true, true),
            Self::Drunkard => JokerCompatibility::new(false, true, true),
            Self::Dusk => JokerCompatibility::new(true, true, true),
            Self::Egg => JokerCompatibility::new(false, true, true),
            Self::Erosion => JokerCompatibility::new(true, true, true),
            Self::EvenSteven => JokerCompatibility::new(true, true, true),
            Self::FacelessJoker => JokerCompatibility::new(true, true, true),
            Self::Fibonacci => JokerCompatibility::new(true, true, true),
            Self::FlashCard => JokerCompatibility::new(true, false, true),
            Self::FlowerPot => JokerCompatibility::new(true, true, true),
            Self::FortuneTeller => JokerCompatibility::new(true, true, true),
            Self::FourFingers => JokerCompatibility::new(false, true, true),
            Self::GiftCard => JokerCompatibility::new(false, true, true),
            Self::GlassJoker => JokerCompatibility::new(true, false, true),
            Self::GluttonousJoker => JokerCompatibility::new(true, true, true),
            Self::GoldenJoker => JokerCompatibility::new(false, true, true),
            Self::GoldenTicket => JokerCompatibility::new(true, true, true),
            Self::GreedyJoker => JokerCompatibility::new(true, true, true),
            Self::GreenJoker => JokerCompatibility::new(true, false, true),
            Self::GrosMichel => JokerCompatibility::new(true, true, false),
            Self::Hack => JokerCompatibility::new(true, true, true),
            Self::HalfJoker => JokerCompatibility::new(true, true, true),
            Self::Hallucination => JokerCompatibility::new(true, true, true),
            Self::HangingChad => JokerCompatibility::new(true, true, true),
            Self::Hiker => JokerCompatibility::new(true, true, true),
            Self::HittheRoad => JokerCompatibility::new(true, true, true),
            Self::Hologram => JokerCompatibility::new(true, false, true),
            Self::IceCream => JokerCompatibility::new(true, true, false),
            Self::InvisibleJoker => JokerCompatibility::new(false, true, false),
            Self::Joker => JokerCompatibility::new(true, true, true),
            Self::JokerStencil => JokerCompatibility::new(true, true, true),
            Self::JollyJoker => JokerCompatibility::new(true, true, true),
            Self::Juggler => JokerCompatibility::new(false, true, true),
            Self::LoyaltyCard(_) => JokerCompatibility::new(true, true, true),
            Self::Luchador => JokerCompatibility::new(true, true, false),
            Self::LuckyCat => JokerCompatibility::new(true, false, true),
            Self::LustyJoker => JokerCompatibility::new(true, true, true),
            Self::MadJoker => JokerCompatibility::new(true, true, true),
            Self::Madness => JokerCompatibility::new(true, false, true),
            Self::MailInRebate => JokerCompatibility::new(true, true, true),
            Self::MarbleJoker => JokerCompatibility::new(true, true, true),
            Self::Matador => JokerCompatibility::new(true, true, true),
            Self::MerryAndy => JokerCompatibility::new(false, true, true),
            Self::MidasMask => JokerCompatibility::new(false, true, true),
            Self::Mime => JokerCompatibility::new(true, true, true),
            Self::Misprint => JokerCompatibility::new(true, true, true),
            Self::MrBones => JokerCompatibility::new(false, true, false),
            Self::MysticSummit => JokerCompatibility::new(true, true, true),
            Self::Obelisk => JokerCompatibility::new(true, false, true),
            Self::OddTodd => JokerCompatibility::new(true, true, true),
            Self::OnyxAgate => JokerCompatibility::new(true, true, true),
            Self::OopsAll6s => JokerCompatibility::new(false, true, true),
            Self::Pareidolia => JokerCompatibility::new(false, true, true),
            Self::Perkeo => JokerCompatibility::new(true, true, true),
            Self::Photograph => JokerCompatibility::new(true, true, true),
            Self::Popcorn => JokerCompatibility::new(true, true, false),
            Self::RaisedFist => JokerCompatibility::new(true, true, true),
            Self::Ramen => JokerCompatibility::new(true, true, false),
            Self::RedCard => JokerCompatibility::new(true, false, true),
            Self::ReservedParking => JokerCompatibility::new(true, true, true),
            Self::RidetheBus => JokerCompatibility::new(true, false, true),
            Self::RiffRaff => JokerCompatibility::new(true, true, true),
            Self::Rocket => JokerCompatibility::new(false, false, true),
            Self::RoughGem => JokerCompatibility::new(true, true, true),
            Self::Runner => JokerCompatibility::new(true, false, true),
            Self::Satellite => JokerCompatibility::new(false, true, true),
            Self::ScaryFace => JokerCompatibility::new(true, true, true),
            Self::Scholar => JokerCompatibility::new(true, true, true),
            Self::SeeingDouble => JokerCompatibility::new(true, true, true),
            Self::Seltzer => JokerCompatibility::new(true, true, false),
            Self::ShoottheMoon => JokerCompatibility::new(true, true, true),
            Self::Shortcut => JokerCompatibility::new(false, true, true),
            Self::Showman => JokerCompatibility::new(false, true, true),
            Self::SixthSense => JokerCompatibility::new(false, true, true),
            Self::SlyJoker => JokerCompatibility::new(true, true, true),
            Self::SmearedJoker => JokerCompatibility::new(false, true, true),
            Self::SmileyFace => JokerCompatibility::new(true, true, true),
            Self::SockandBuskin => JokerCompatibility::new(true, true, true),
            Self::SpaceJoker => JokerCompatibility::new(true, true, true),
            Self::SpareTrousers => JokerCompatibility::new(true, false, true),
            Self::Splash => JokerCompatibility::new(false, true, true),
            Self::SquareJoker => JokerCompatibility::new(true, false, true),
            Self::SteelJoker => JokerCompatibility::new(true, true, true),
            Self::StoneJoker => JokerCompatibility::new(true, true, true),
            Self::Stuntman => JokerCompatibility::new(true, true, true),
            Self::Supernova => JokerCompatibility::new(true, true, true),
            Self::Superposition => JokerCompatibility::new(true, true, true),
            Self::Swashbuckler => JokerCompatibility::new(true, true, true),
            Self::Séance => JokerCompatibility::new(true, true, true),
            Self::TheDuo => JokerCompatibility::new(true, true, true),
            Self::TheFamily => JokerCompatibility::new(true, true, true),
            Self::TheIdol => JokerCompatibility::new(true, true, true),
            Self::TheOrder => JokerCompatibility::new(true, true, true),
            Self::TheTribe => JokerCompatibility::new(true, true, true),
            Self::TheTrio => JokerCompatibility::new(true, true, true),
            Self::Throwback => JokerCompatibility::new(true, true, true),
            Self::ToDoList => JokerCompatibility::new(true, true, true),
            Self::TotheMoon => JokerCompatibility::new(false, true, true),
            Self::TradingCard => JokerCompatibility::new(false, true, true),
            Self::Triboulet => JokerCompatibility::new(true, true, true),
            Self::Troubadour => JokerCompatibility::new(false, true, true),
            Self::TurtleBean => JokerCompatibility::new(false, true, false),
            Self::Vagabond => JokerCompatibility::new(true, true, true),
            Self::Vampire => JokerCompatibility::new(true, false, true),
            Self::WalkieTalkie => JokerCompatibility::new(true, true, true),
            Self::WeeJoker => JokerCompatibility::new(true, false, true),
            Self::WilyJoker => JokerCompatibility::new(true, true, true),
            Self::WrathfulJoker => JokerCompatibility::new(true, true, true),
            Self::Yorick => JokerCompatibility::new(true, true, true),
            Self::ZanyJoker => JokerCompatibility::new(true, true, true),
        }
    }
}
impl Name for JokerType {
    fn name(&self) -> &'static str {
        match self {
            Self::EightBall => "8 Ball",
            Self::AbstractJoker => "Abstract Joker",
            Self::Acrobat => "Acrobat",
            Self::AncientJoker => "Ancient Joker",
            Self::Arrowhead => "Arrowhead",
            Self::Astronomer => "Astronomer",
            Self::Banner => "Banner",
            Self::Baron => "Baron",
            Self::BaseballCard => "Baseball Card",
            Self::Blackboard => "Blackboard",
            Self::Bloodstone => "Bloodstone",
            Self::BlueJoker => "Blue Joker",
            Self::Blueprint => "Blueprint",
            Self::Bootstraps => "Bootstraps",
            Self::Brainstorm => "Brainstorm",
            Self::Bull => "Bull",
            Self::Burglar => "Burglar",
            Self::BurntJoker => "Burnt Joker",
            Self::BusinessCard => "Business Card",
            Self::Campfire => "Campfire",
            Self::Canio => "Canio",
            Self::CardSharp => "Card Sharp",
            Self::Cartomancer => "Cartomancer",
            Self::Castle => "Castle",
            Self::Cavendish => "Cavendish",
            Self::CeremonialDagger(_) => "Ceremonial Dagger",
            Self::Certificate => "Certificate",
            Self::ChaostheClown => "Chaos the Clown",
            Self::Chicot => "Chicot",
            Self::CleverJoker => "Clever Joker",
            Self::Cloud9 => "Cloud 9",
            Self::Constellation => "Constellation",
            Self::CraftyJoker => "Crafty Joker",
            Self::CrazyJoker => "Crazy Joker",
            Self::CreditCard => "Credit Card",
            Self::DelayedGratification => "Delayed Gratification",
            Self::DeviousJoker => "Devious Joker",
            Self::DietCola => "Diet Cola",
            Self::DNA => "DNA",
            Self::DriversLicense => "Driver's License",
            Self::DrollJoker => "Droll Joker",
            Self::Drunkard => "Drunkard",
            Self::Dusk => "Dusk",
            Self::Egg => "Egg",
            Self::Erosion => "Erosion",
            Self::EvenSteven => "Even Steven",
            Self::FacelessJoker => "Faceless Joker",
            Self::Fibonacci => "Fibonacci",
            Self::FlashCard => "Flash Card",
            Self::FlowerPot => "Flower Pot",
            Self::FortuneTeller => "Fortune Teller",
            Self::FourFingers => "Four Fingers",
            Self::GiftCard => "Gift Card",
            Self::GlassJoker => "Glass Joker",
            Self::GluttonousJoker => "Gluttonous Joker",
            Self::GoldenJoker => "Golden Joker",
            Self::GoldenTicket => "Golden Ticket",
            Self::GreedyJoker => "Greedy Joker",
            Self::GreenJoker => "Green Joker",
            Self::GrosMichel => "Gros Michel",
            Self::Hack => "Hack",
            Self::HalfJoker => "Half Joker",
            Self::Hallucination => "Hallucination",
            Self::HangingChad => "Hanging Chad",
            Self::Hiker => "Hiker",
            Self::HittheRoad => "Hit the Road",
            Self::Hologram => "Hologram",
            Self::IceCream => "Ice Cream",
            Self::InvisibleJoker => "Invisible Joker",
            Self::Joker => "Joker",
            Self::JokerStencil => "Joker Stencil",
            Self::JollyJoker => "Jolly Joker",
            Self::Juggler => "Juggler",
            Self::LoyaltyCard(_) => "Loyalty Card",
            Self::Luchador => "Luchador",
            Self::LuckyCat => "Lucky Cat",
            Self::LustyJoker => "Lusty Joker",
            Self::MadJoker => "Mad Joker",
            Self::Madness => "Madness",
            Self::MailInRebate => "Mail-In Rebate",
            Self::MarbleJoker => "Marble Joker",
            Self::Matador => "Matador",
            Self::MerryAndy => "Merry Andy",
            Self::MidasMask => "Midas Mask",
            Self::Mime => "Mime",
            Self::Misprint => "Misprint",
            Self::MrBones => "Mr. Bones",
            Self::MysticSummit => "Mystic Summit",
            Self::Obelisk => "Obelisk",
            Self::OddTodd => "Odd Todd",
            Self::OnyxAgate => "Onyx Agate",
            Self::OopsAll6s => "Oops! All 6s",
            Self::Pareidolia => "Pareidolia",
            Self::Perkeo => "Perkeo",
            Self::Photograph => "Photograph",
            Self::Popcorn => "Popcorn",
            Self::RaisedFist => "Raised Fist",
            Self::Ramen => "Ramen",
            Self::RedCard => "Red Card",
            Self::ReservedParking => "Reserved Parking",
            Self::RidetheBus => "Ride the Bus",
            Self::RiffRaff => "Riff-Raff",
            Self::Rocket => "Rocket",
            Self::RoughGem => "Rough Gem",
            Self::Runner => "Runner",
            Self::Satellite => "Satellite",
            Self::ScaryFace => "Scary Face",
            Self::Scholar => "Scholar",
            Self::SeeingDouble => "Seeing Double",
            Self::Seltzer => "Seltzer",
            Self::ShoottheMoon => "Shoot the Moon",
            Self::Shortcut => "Shortcut",
            Self::Showman => "Showman",
            Self::SixthSense => "Sixth Sense",
            Self::SlyJoker => "Sly Joker",
            Self::SmearedJoker => "Smeared Joker",
            Self::SmileyFace => "Smiley Face",
            Self::SockandBuskin => "Sock and Buskin",
            Self::SpaceJoker => "Space Joker",
            Self::SpareTrousers => "Spare Trousers",
            Self::Splash => "Splash",
            Self::SquareJoker => "Square Joker",
            Self::SteelJoker => "Steel Joker",
            Self::StoneJoker => "Stone Joker",
            Self::Stuntman => "Stuntman",
            Self::Supernova => "Supernova",
            Self::Superposition => "Superposition",
            Self::Swashbuckler => "Swashbuckler",
            Self::Séance => "Séance",
            Self::TheDuo => "The Duo",
            Self::TheFamily => "The Family",
            Self::TheIdol => "The Idol",
            Self::TheOrder => "The Order",
            Self::TheTribe => "The Tribe",
            Self::TheTrio => "The Trio",
            Self::Throwback => "Throwback",
            Self::ToDoList => "To Do List",
            Self::TotheMoon => "To the Moon",
            Self::TradingCard => "Trading Card",
            Self::Triboulet => "Triboulet",
            Self::Troubadour => "Troubadour",
            Self::TurtleBean => "Turtle Bean",
            Self::Vagabond => "Vagabond",
            Self::Vampire => "Vampire",
            Self::WalkieTalkie => "Walkie Talkie",
            Self::WeeJoker => "Wee Joker",
            Self::WilyJoker => "Wily Joker",
            Self::WrathfulJoker => "Wrathful Jonkler",
            Self::Yorick => "Yorick",
            Self::ZanyJoker => "Zany Joker",
        }
    }
}
impl Price for JokerType {
    fn buy_price(&self) -> u8 {
        match self {
            Self::EightBall => 5,
            Self::AbstractJoker => 4,
            Self::Acrobat => 6,
            Self::AncientJoker => 8,
            Self::Arrowhead => 7,
            Self::Astronomer => 8,
            Self::Banner => 5,
            Self::Baron => 8,
            Self::BaseballCard => 8,
            Self::Blackboard => 6,
            Self::Bloodstone => 7,
            Self::BlueJoker => 5,
            Self::Blueprint => 10,
            Self::Bootstraps => 7,
            Self::Brainstorm => 10,
            Self::Bull => 6,
            Self::Burglar => 6,
            Self::BurntJoker => 8,
            Self::BusinessCard => 4,
            Self::Campfire => 9,
            Self::Canio => 20,
            Self::CardSharp => 6,
            Self::Cartomancer => 6,
            Self::Castle => 6,
            Self::Cavendish => 4,
            Self::CeremonialDagger(_) => 6,
            Self::Certificate => 6,
            Self::ChaostheClown => 4,
            Self::Chicot => 20,
            Self::CleverJoker => 4,
            Self::Cloud9 => 7,
            Self::Constellation => 6,
            Self::CraftyJoker => 4,
            Self::CrazyJoker => 4,
            Self::CreditCard => 1,
            Self::DelayedGratification => 4,
            Self::DeviousJoker => 4,
            Self::DietCola => 6,
            Self::DNA => 8,
            Self::DriversLicense => 7,
            Self::DrollJoker => 4,
            Self::Drunkard => 4,
            Self::Dusk => 5,
            Self::Egg => 4,
            Self::Erosion => 6,
            Self::EvenSteven => 4,
            Self::FacelessJoker => 4,
            Self::Fibonacci => 8,
            Self::FlashCard => 5,
            Self::FlowerPot => 6,
            Self::FortuneTeller => 6,
            Self::FourFingers => 7,
            Self::GiftCard => 6,
            Self::GlassJoker => 6,
            Self::GluttonousJoker => 5,
            Self::GoldenJoker => 6,
            Self::GoldenTicket => 5,
            Self::GreedyJoker => 5,
            Self::GreenJoker => 4,
            Self::GrosMichel => 5,
            Self::Hack => 6,
            Self::HalfJoker => 5,
            Self::Hallucination => 4,
            Self::HangingChad => 4,
            Self::Hiker => 5,
            Self::HittheRoad => 8,
            Self::Hologram => 7,
            Self::IceCream => 5,
            Self::InvisibleJoker => 8,
            Self::Joker => 2,
            Self::JokerStencil => 8,
            Self::JollyJoker => 3,
            Self::Juggler => 4,
            Self::LoyaltyCard(_) => 5,
            Self::Luchador => 5,
            Self::LuckyCat => 6,
            Self::LustyJoker => 5,
            Self::MadJoker => 4,
            Self::Madness => 7,
            Self::MailInRebate => 4,
            Self::MarbleJoker => 6,
            Self::Matador => 7,
            Self::MerryAndy => 7,
            Self::MidasMask => 7,
            Self::Mime => 5,
            Self::Misprint => 4,
            Self::MrBones => 5,
            Self::MysticSummit => 5,
            Self::Obelisk => 8,
            Self::OddTodd => 4,
            Self::OnyxAgate => 7,
            Self::OopsAll6s => 4,
            Self::Pareidolia => 5,
            Self::Perkeo => 20,
            Self::Photograph => 5,
            Self::Popcorn => 5,
            Self::RaisedFist => 5,
            Self::Ramen => 6,
            Self::RedCard => 5,
            Self::ReservedParking => 6,
            Self::RidetheBus => 6,
            Self::RiffRaff => 6,
            Self::Rocket => 6,
            Self::RoughGem => 7,
            Self::Runner => 5,
            Self::Satellite => 6,
            Self::ScaryFace => 4,
            Self::Scholar => 4,
            Self::SeeingDouble => 6,
            Self::Seltzer => 6,
            Self::ShoottheMoon => 5,
            Self::Shortcut => 7,
            Self::Showman => 5,
            Self::SixthSense => 6,
            Self::SlyJoker => 3,
            Self::SmearedJoker => 7,
            Self::SmileyFace => 4,
            Self::SockandBuskin => 6,
            Self::SpaceJoker => 5,
            Self::SpareTrousers => 6,
            Self::Splash => 3,
            Self::SquareJoker => 4,
            Self::SteelJoker => 7,
            Self::StoneJoker => 6,
            Self::Stuntman => 7,
            Self::Supernova => 5,
            Self::Superposition => 4,
            Self::Swashbuckler => 4,
            Self::Séance => 6,
            Self::TheDuo => 8,
            Self::TheFamily => 8,
            Self::TheIdol => 6,
            Self::TheOrder => 8,
            Self::TheTribe => 8,
            Self::TheTrio => 8,
            Self::Throwback => 6,
            Self::ToDoList => 4,
            Self::TotheMoon => 5,
            Self::TradingCard => 6,
            Self::Triboulet => 20,
            Self::Troubadour => 6,
            Self::TurtleBean => 6,
            Self::Vagabond => 8,
            Self::Vampire => 7,
            Self::WalkieTalkie => 4,
            Self::WeeJoker => 8,
            Self::WilyJoker => 4,
            Self::WrathfulJoker => 5,
            Self::Yorick => 20,
            Self::ZanyJoker => 4,
        }
    }
}

// CODEGEN END
