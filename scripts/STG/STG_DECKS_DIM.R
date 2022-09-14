#STG_DECKS_DIM
required_data("SRC_DECKS_DIM")
STG_DECKS_DIM <- SRC_DECKS_DIM[Side != -1, .(Divari = as.numeric(Divari),
                                   Picked = as.numeric(Picked),
                                   Omistaja_ID = str_sub(Omistaja_nimi, 1, 1),
                                   Pakka_No = as.numeric(Pakka),
                                   Pakka_ID = as.numeric(rivi_id),
                                   Nimi,
                                   Retired = as.numeric(Retired),
                                   Side = as.numeric(Side),
                                   Omistaja_NM = Omistaja_nimi,
                                   NineSide = ifelse(Retired == 1 & Side == 1, 1, 0))]
