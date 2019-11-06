#STG_CARDS_DIM
required_data("SRC_CARDS_DIM")
temp <- SRC_CARDS_DIM[, .(MID = as.numeric(MID),
                                   Card_ID = as.numeric(Card_ID),
                                   Name = iconv(x = Name, to = "UTF-8"),
                                   Text,
                                   Cost,
                                   Converted_Cost = as.numeric(Converted_Cost),
                                   Rarity,
                                   Colors,
                                   Stats)]

#fix land MIDs

landitaulu <- data.table(Name = c("Forest","Swamp","Plains",                                  "Mountain",
                                  "Island",
                                  "Wastes")
                         ,  land_MID = c(289327,
                                         473220,
                                         473212,
                                         221305,
                                         386333,
                                         407693))
joinlandi <- landitaulu[temp, on = "Name"]
joinlandi[, MID := ifelse(is.na(land_MID), MID, land_MID)]
joinlandi[, land_MID := NULL]
STG_CARDS_DIM <- joinlandi
