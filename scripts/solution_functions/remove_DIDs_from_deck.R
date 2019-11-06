remove_DIDs_from_deck <- function(input_Pakka_ID, removed_DIDs, STG_CARDS, con) {

  old_dl <- STG_CARDS[Pakka_form_ID == STG_CARDS[Pakka_ID == input_Pakka_ID, max(Pakka_form_ID)]]
  new_dl <- old_dl[!DRAFT_CARDS_ID %in% removed_DIDs]
  free_pfi <- dbQ("SELECT max(Pakka_form_ID) FROM CARDS", con) + 1
  new_dl[, Pakka_form_ID := free_pfi]
  new_dl[, monesko_kortti := NULL]
  return(new_dl)
}
