#STG_CARDS_DIM
required_data("SRC_CARDS_DIM")
STG_CARDS_DIM <- SRC_CARDS_DIM[, .(MID = as.numeric(MID),
                                   Card_ID = as.numeric(Card_ID),
                                   Name,
                                   Text,
                                   Cost,
                                   Converted_Cost = as.numeric(Converted_Cost),
                                   Rarity,
                                   Colors,
                                   Stats)]
