#one time fix to combine sides
required_data(c("SRC_DECKS_DIM", "SRC_CARDS"))
SRC_DECKS_DIM

con <- connDB(con)
cards_raw <- dbSelectAll("CARDS", con)

varisidet <- SRC_DECKS_DIM[Side == -1, .(Pakka_ID = rivi_id, Nimi, Omistaja)]

kokoSidet <- SRC_DECKS_DIM[Side == -1]


#lähetää rakentaa sidejä
aggr_PFI <- cards_raw[, .(Valid_from_DT = max(Valid_from_DT)), by = .(Pakka_ID, Pakka_form_ID)]
sortedr <- aggr_PFI[order(Valid_from_DT)]
sortedr[, Pakka_ID := as.character(Pakka_ID)]
join_omistaja <- sortedr[varisidet, on = "Pakka_ID"]
#otetaa vaan yks havainto per päivä
join_omistaja[, paiva := as.IDate(Valid_from_DT)]
join_omistaja <- join_omistaja[order(Valid_from_DT)]
#max_per_paiva <- join_omistaja[join_omistaja[, .I[which.max(Pakka_form_ID)], by= .(paiva, Omistaja)]$V1]
#sortattu_paiva <-max_per_paiva[order(paiva)]


snap_shot_decklists <- NULL
rolling_decklist <- join_omistaja[1 == 0]
for (pfi_rivi in 1:nrow(join_omistaja)) {
  rivitiedot <- join_omistaja[pfi_rivi]
  #poista vanha
  rolling_decklist <- rolling_decklist[!Pakka_ID %in% rivitiedot[,Pakka_ID] ]
  #lisää uus
  rolling_decklist <- rbind(rolling_decklist, join_omistaja[Pakka_form_ID %in% rivitiedot[,Pakka_form_ID]], fill = TRUE)
  rolling_decklist[, Deck_version := pfi_rivi]
  snap_shot_decklists <- rbind(snap_shot_decklists, rolling_decklist)
}
#ota joka omistajan max pfi per paiva
max_pfi_from_snapshot <-  snap_shot_decklists[snap_shot_decklists[, .I[which.max(Pakka_form_ID)], by= .(paiva, Omistaja)]$V1]
max_pfi_from_snapshot[order(paiva)]
new_deck_data <- max_pfi_from_snapshot[, .(new_PID = ifelse(Omistaja == 1, 21, 22),
                                           new_PFI = 0,
                                           Valid_from_DT_new = Valid_from_DT,
                                           Deck_version,
                                           Omistaja)]

#get pfis that belong to version
jion_versions <- snap_shot_decklists[new_deck_data, on = .(Deck_version, Omistaja)]
jion_versions[, .N, by = .(Omistaja, Deck_version)]
#and join actuals cards
join_decklists <- cards_raw[jion_versions, on = "Pakka_form_ID"]
vapaa_pfi_eka <- cards_raw[, max(Pakka_form_ID)]
clean_cols <- join_decklists[, .(MID,
                                 Pakka_ID = new_PID,
                                 DRAFT_CARDS_ID,
                                 #Pakka_form_ID = seq_len(.N) + vapaa_pfi_eka,
                                 Card_ID,
                                 Name,
                                 Maindeck,
                                 Valid_from_DT = Valid_from_DT_new,
                                 Deck_version,
                                 Omistaja)]
calc_new_pfi <- clean_cols[, .(.N), by = .(Omistaja, Deck_version)]
calc_new_pfi[, Pakka_form_ID := vapaa_pfi_eka + seq_len(.N)]
join_new_pfi <- calc_new_pfi[clean_cols, on = .(Deck_version, Omistaja)]
join_new_pfi[, ':=' (Omistaja = NULL,
                     Deck_version = NULL,
                     N = NULL)]
#append_to_data
#dbWriteTable(con, "CARDS", join_new_pfi, append = TRUE, row.names = FALSE)




#max_pfi_from_snapshot <-  snap_shot_decklists[, .(PFIs = list(Pakka_form_ID)), by = .(paiva, Omistaja)]

#max_pfi_from_snapshot[paiva == "2019-12-08"]

varisidet[, uus_id := ifelse(Omistaja_ID == "L", 21, 22)]

maxit <- cards_raw[cards_raw[, .I[which.max(Pakka_form_ID)], by= Pakka_ID]$V1]
vapaa_pfi <- cards_raw[, max(Pakka_form_ID)]
sidet <- maxit[varisidet, on = "Pakka_ID"]#maxit[Pakka_ID %in% varisidet[,Pakka_ID]]

sscols_update <- sidet[, .(Pakka_form_ID, uus_id, uus_pfi = ifelse(Omistaja_ID == "L", vapaa_pfi + 1, vapaa_pfi + 2))]

joinaa_cards <- cards_raw[sscols_update, on = "Pakka_form_ID"]

joinaa_cards[, ':=' (Pakka_form_ID = uus_pfi,
                     Pakka_ID = uus_id,
                     uus_id = NULL,
                     uus_pfi = NULL)]

#append_to_data
#dbWriteTable(con, "CARDS", joinaa_cards, append = TRUE, row.names = FALSE)

#update DECKS
#dbQ("UPDATE DECKS_DIM set Side = -1 where Pakka < 0", con)
#dbQ("UPDATE DECKS_DIM set Side = 1 where Pakka = 0", con)
