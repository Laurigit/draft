#add card to decklist
# new_MIDs <- c(370424,
#               370425,
#               370426)

createNewDecklist_after_draft <- function(new_DCIDs, Pakka_ID_input, STG_CARDS, STG_CARDS_DIM, STG_DRAFT_CARDS, STG_DECKS_DIM) {

   old_PFI <- STG_CARDS[Pakka_ID == Pakka_ID_input, max(Pakka_form_ID)]
  old_dl <- STG_CARDS[Pakka_form_ID == old_PFI, .(MID, Pakka_ID, Name, Maindeck, DRAFT_CARDS_ID)]
  new_MIDs <- STG_DRAFT_CARDS[DRAFT_CARDS_ID %in% new_DCIDs[, DRAFT_CARDS_ID], .(MID, DRAFT_CARDS_ID)]
  new_names <- STG_CARDS_DIM[MID %in% new_MIDs[, MID], .(MID, Name)]

  if (STG_DECKS_DIM[Pakka_ID == Pakka_ID_input, NineSide] == 1) {
    toMainorSide <- 1
  } else {
    toMainorSide <- 0
  }

  join_names <- new_names[new_MIDs, on = "MID"]
  join_names[, ':=' (Maindeck = toMainorSide, Pakka_ID = Pakka_ID_input)]
  new_dl <- rbind(old_dl, join_names)

  #new pfi
  new_pfi <-  STG_CARDS[, max(Pakka_form_ID)] + 1
  new_dl[, Pakka_form_ID := new_pfi]
  print(new_dl)
#  aggregate <- new_dl[, .(Count = sum(Count), MID = max(MID)), by = .(Name, Maindeck, Pakka_form_ID, Pakka_ID)]


  return(new_dl)
}

