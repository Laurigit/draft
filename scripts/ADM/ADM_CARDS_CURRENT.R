#ADM_CARDS_CURRENT

required_data(c("STG_CARDS", "STAT_SIDE_CARD_AGE", "STG_DECKS_DIM"))

curr_decks <- STG_DECKS_DIM[Retired == 0 | (Retired == 1 & Side == 1)]

max_pfi <- STG_CARDS[, .(maxpfi = max(Pakka_form_ID)), by  = Pakka_ID]

join_current_decks <- max_pfi[curr_decks, on = "Pakka_ID"]

latest_dl <- STG_CARDS[Pakka_form_ID %in% join_current_decks[, maxpfi]]


#yritetään lisätä ikä
sscols <- STAT_SIDE_CARD_AGE[, .(Pakka_ID, Pakka_form_ID, Omistaja_ID, Name, Card_age, Count, Maindeck)]
#levita
levita_age <- sscols[rep(seq_len(nrow(sscols)), Count), ][, Count := NULL]
levita_age[, Name_count := seq_len(.N), by = .(Name, Pakka_ID, Maindeck)]
sscols_age <- levita_age[, .(Maindeck,Name, Card_age, Name_count, Pakka_ID = as.numeric(Pakka_ID))]

joinaa <- sscols_age[latest_dl, on = .(Pakka_ID = Pakka_ID,
                                       Name_count = monesko_kortti,
                                       Maindeck = Maindeck,
                                       Name = Name)]

res_cols <- joinaa[,. (Maindeck, Name, Card_age,  Pakka_ID, MID, Pakka_form_ID, DRAFT_CARDS_ID)]
#sort so that oldest is first, then count monesko_kortti again
sorted <- res_cols[order(Pakka_ID, Pakka_form_ID, Name, Maindeck, -Card_age)]
sorted[, monesko_kortti := seq_len(.N), by = .(Name, Pakka_form_ID, Maindeck)]

ADM_CARDS_CURRENT <- sorted
