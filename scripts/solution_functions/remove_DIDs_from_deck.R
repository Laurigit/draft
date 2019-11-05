remove_DIDs_from_deck <- function(input_Pakka_ID, removed_DIDs, STG_CARDS, STG_DECKS_DIM, con) {
  old_dl <- STG_CARDS[Pakka_form_ID == STG_DECKS_DIM[Pakka_ID == input_Pakka_ID, max(Pakka_form_ID)]]
  new_dl <- old_dl[!DRAFT_CARDS_ID %in% removed_DIDs]
  free_pfi <- dbQ("SELECT max(Pakka_form_ID) FROM DECKS_DIM", con) + 1
  new_dl[, Pakka_form_ID := free_pfi]
  return(new_dl)
}
