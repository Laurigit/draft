#ADM_CURRENT_PAKKA
required_data("STG_CARDS", "STG_DECKS_DIM")
max_pfi <- STG_CARDS[, .(Pakka_form_ID = max(Pakka_form_ID)), by = Pakka_ID]
sscolsDecks <- STG_DECKS_DIM[(Retired == 1 & Side == 1) | (Retired == 0 & Side == 0), .(Pakka_ID, Pakka_Name = Nimi)]
joinDeks <- max_pfi[sscolsDecks, on = "Pakka_ID"]
ADM_CURRENT_PAKKA <- joinDeks
