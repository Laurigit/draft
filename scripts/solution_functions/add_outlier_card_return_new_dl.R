add_outlier_card_return_new_dl <- function(input_card_name, Pakka_ID_input, STG_CARDS, STG_CARDS_DIM, STG_DECKS_DIM, con) {

  old_PFI <- STG_CARDS[Pakka_ID == Pakka_ID_input, max(Pakka_form_ID)]
  old_dl <- STG_CARDS[Pakka_form_ID == old_PFI, .(MID, Pakka_ID, Name, Maindeck, DRAFT_CARDS_ID)]
  join_names <- STG_CARDS_DIM[Name %in% input_card_name, .(MID, Name)]

  if (STG_DECKS_DIM[Pakka_ID == Pakka_ID_input, NineSide] == 1) {
    toMainorSide <- 1
  } else {
    toMainorSide <- 0
  }

  #free dcid
  fDID <- as.numeric(dbQ("SELECT MIN(DRAFT_CARDS_ID) FROM CARDS", con) - 1)

  join_names[, ':=' (Maindeck = toMainorSide, Pakka_ID = Pakka_ID_input, DRAFT_CARDS_ID = fDID)]
  new_dl <- rbind(old_dl, join_names)

  #new pfi
  new_pfi <- as.numeric(dbQ("SELECT MAX(PAKKA_FORM_ID) FROM CARDS", con)) + 1
  new_dl[, Pakka_form_ID := new_pfi]
  print(new_dl)
  #  aggregate <- new_dl[, .(Count = sum(Count), MID = max(MID)), by = .(Name, Maindeck, Pakka_form_ID, Pakka_ID)]


  return(new_dl)
}
