Fetches joker info from fandom and generates the `JokerType` enum as well as its methods `name`, `rarity`, `buy_price`, `sell_price`, `effect_type` and `compatibility` from it. Also fetches assets.
Implemented as a separate crate instead of a proc macro to avoid having to have an internet connection when building and to avoid constantly regenerating the same info.
