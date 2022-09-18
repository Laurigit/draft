#hopefulle onetime fix to get new_DRAFT_CARDS_ID for basic lands that positive DRAFT_CARDS_ID

required_data("SRC_CARDS")


#free draft_cdid
min_cdid <- SRC_CARDS[, min(DRAFT_CARDS_ID)]

wrong_did <- SRC_CARDS[Name %in% c("Mountain", "Plains", "Forest", "Swamp", "Island") & DRAFT_CARDS_ID > 0]

aggr_wrong <- wrong_did[, .N ,by = .(Pakka_ID, DRAFT_CARDS_ID)]
aggr_wrong[, new_DID := min_cdid - seq_len(.N)]
sscols <-aggr_wrong[ ,.(Pakka_ID, DRAFT_CARDS_ID, new_DID)]


join_new <- sscols[SRC_CARDS, on = .(Pakka_ID, DRAFT_CARDS_ID)]

join_new[, DRAFT_CARDS_ID := ifelse(is.na(new_DID), DRAFT_CARDS_ID, new_DID)]
join_new[, new_DID := NULL]
only_old <- join_new[Valid_from_DT < "2020-02-05"]

#bu ol

#dbQ("CREATE TABLE cards_bu_feb LIKE CARDS", con)
#dbQ("INSERT cards_bu_feb SELECT * FROM CARDS", con)
#
# dbQ("DELETE FROM CARDS WHERE 1 = 1", con)
# dbWriteTable(con, "CARDS", only_old, append = TRUE, row.names = FALSE)
