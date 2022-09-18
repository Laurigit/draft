required_data("STG_CARDS")
broken_DID <- STG_CARDS[Name %in% c("Mountain", "Forest", "Plains", "Swamp", "Island") & DRAFT_CARDS_ID > 0]

free_DID <- dbQ("SELECT min(DRAFT_CARDS_ID) as MIN FROM CARDS", con)

broken_DID_agg <- broken_DID[,.N, by = (DRAFT_CARDS_ID)]
broken_DID_agg[, rivi := seq_len(.N)]
broken_DID_agg[, new_DID := free_DID - rivi, by = rivi]
broken_DID_agg[, N := NULL]
broken_DID_agg[, rivi := NULL]

raw_old_cards <- dbSelectAll("CARDS", con)

#join new
join_update_DID <- broken_DID_agg[raw_old_cards, on = "DRAFT_CARDS_ID"]
#fix
join_update_DID[Name %in% c("Mountain", "Forest", "Plains", "Swamp", "Island"), DRAFT_CARDS_ID := ifelse(is.na(new_DID), DRAFT_CARDS_ID, new_DID)]
join_update_DID[, new_DID := NULL]

# dbQ("SHOW TABLES", con)
# dbQ("DELETE FROM CARDS WHERE 1 = 1", con)
# dbWriteTable(con, "CARDS", join_update_DID, append = TRUE, overwrite = FALSE, row.names = FALSE)
