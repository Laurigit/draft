#STG_DECKS_DIM
required_data("SRC_DECKS_DIM")
STG_DECKS_DIM <- SRC_DECKS_DIM[, .(Divari = as.numeric(Divari),
                                   Picked = as.numeric(Picked),
                                   Omistaja_ID,
                                   Pakka_ID = as.numeric(rivi_id),
                                   Nimi,
                                   Retired = as.numeric(Retired),
                                   Side = as.numeric(Side),
                                   Omistaja_NM = Omistaja_nimi)]
