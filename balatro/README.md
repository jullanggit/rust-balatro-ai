## States
### Selecting Game
- Selecting Deck
- Selecting Stake
### In Game
- Selecting Blind
- In Round
- Cashing Out
- In Shop

## Data
### Selecting Game
#### Selecting Deck
---
#### Selecting Stake
Selecting Deck
+ Deck
### In Game
Selecting Stake
+ Blind
+ Ante
+ Tags
+ Money
+ Num Hands
+ Num Discards
+ Jokers
+ Joker slots
+ Consumeables
+ Consumeable slots
+ Vouchers
+ Tag Queue
+ Deck Cards
+ Hand size
+ Hand Levels
#### Selecting Blind
In Game
#### In Round
In Game
+ Hand Cards
+ Used Cards
+ Required Score
+ Current score
##### Scoring
In Round
+ Chips
+ Mult
#### Cashing Out
In Game
+ Money gained
#### In Shop
In Game
+ Vouchers
+ Packs
+ Shop items
+ Reroll cost
#### Opening Pack
Selecting Blind | In Round | Cashing Out | In Shop
+ Pack Options
+ Remaining Choices

## Actions
### Selecting Game
#### Selecting Deck
+ Select Deck
-> Transition to Selecting Stake, set Deck
#### Selecting Stake
+ Select Stake
-> Transition to Selecting Blind, set Stake
### In Game
+ Open Pack
-> Transition to sub-state Opening Pack
+ Use Consumeable
+ Sell Joker
+ Sell Consumable
+ Move Joker
#### Selecting Blind
In Game
+ Select Blind
+ Skip Blind
+ Reroll Boss Blind
#### In Round
In Game
+ Play Hand
+ Discard Hand
#### In Shop
In Game
+ Buy Item
+ Redeem Voucher
+ Open Pack
+ Reroll
+ Next Round
#### Opening Pack
Selecting Blind | In Round | In Shop
+ Choose Pack Item
+ Skip Pack
