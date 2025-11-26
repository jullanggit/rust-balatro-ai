## Input
Cards, Globals, Jokers, Shop into separate MLP's -> embeddings
Card-emb, Joker-emb into SetAttention
Card-att, joker-att, globals-emb, shop-emb into crossattention(q=jokers, k/v=cards+globals+shop)
card-att, joker-cross, globals-emb, shop-emb into pool
card-pool, joker-pool, globals-pool, shop-pool into concat
