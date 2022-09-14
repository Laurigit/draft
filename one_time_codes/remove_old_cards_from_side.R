# required_data(c("ADM_CARDS_CURRENT", "STG_DECKS_DIM"))
#  age <- ADM_CARDS_CURRENT[ Card_age > 25, .(DRAFT_CARDS_ID, Pakka_ID, Name)]
# # age <- ADM_CARDS_CURRENT[ Card_age > 25 & Pakka_ID == 21, .(DRAFT_CARDS_ID, Pakka_ID, Name)]
#
#  sides <- STG_DECKS_DIM[Omistaja_NM == "Lauri" & NineSide == 1, .N, by = Pakka_ID][, Pakka_ID]
#
# for(update_sides in sides) {
#   remove <- age[Pakka_ID == update_sides, DRAFT_CARDS_ID]
#   new_side <- remove_DIDs_from_deck(update_sides, remove, STG_CARDS, con)
#   new_side[, Valid_from_DT := now(tz = "EET")]
#   dbWriteTable(con, "CARDS", new_side, append = TRUE, row.names = FALSE)
#   updateData("SRC_CARDS", ADM_DI_HIERARKIA, globalenv())
# }
