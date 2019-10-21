#add card to decklist
# new_MIDs <- c(370424,
#               370425,
#               370426)

# deck <- reactiveValues(changes = data.table(source = character(),
#                                             MID = numeric(),
#                                             Pakka_ID = numeric(),
#                                             DRAFT_CARDS_ID = numeric(),
#                                             Maindeck = as.character(),
#                                             Removed_from_game = as.numeric()))

#add card to decklist
# new_MIDs <- c(370424,
#               370425,
#               370426)

createNewDecklist_after_changes <- function(new_DCIDs, removed_DCIDs, Pakka_ID_input, STG_CARDS, STG_CARDS_DIM, STG_DRAFT_CARDS) {
  old_PFI <- STG_CARDS[Pakka_ID == Pakka_ID_input, max(Pakka_form_ID)]
  old_dl <- STG_CARDS[Pakka_form_ID == old_PFI, .(MID, Pakka_ID, Name, Maindeck, DRAFT_CARDS_ID, Name)]
  #move to maindeck
  old_dl[DRAFT_CARDS_ID %in% new_DCIDs, Maindeck := 1]

  #remove cards
  new_dl <- old_dl[!DRAFT_CARDS_ID %in% removed_DCIDs]

  #new pfi
  new_pfi <-  STG_CARDS[, max(Pakka_form_ID)] + 1
  new_dl[, Pakka_form_ID := new_pfi]
  print(new_dl)
  #  aggregate <- new_dl[, .(Count = sum(Count), MID = max(MID)), by = .(Name, Maindeck, Pakka_form_ID, Pakka_ID)]


  return(new_dl)
}


