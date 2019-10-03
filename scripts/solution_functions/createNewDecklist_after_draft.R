#add card to decklist
# new_MIDs <- c(370424,
#               370425,
#               370426)

createNewDecklist_after_draft <- function(new_MIDs, Pakka_ID_input, STG_CARDS, STG_CARDS_DIM, con) {
  old_PFI <- STG_CARDS[Pakka_ID == Pakka_ID_input, max(Pakka_form_ID)]
  old_dl <- STG_CARDS[Pakka_form_ID == old_PFI, .(MID, Pakka_ID, Count, Name, Maindeck)]
  new_names <- STG_CARDS_DIM[MID %in% new_MIDs[, MID], .(MID, Name)]
  new_MIDs[, MID := as.numeric(MID)]
  join_names <- new_names[new_MIDs, on = "MID"]
  join_names[, ':=' (Maindeck = FALSE, Pakka_ID = Pakka_ID_input,
                    Count = 1)]
  new_dl <- rbind(old_dl, join_names, fill = TRUE)

  #new pfi
  new_pfi <-  STG_CARDS[, max(Pakka_form_ID)] + 1
  new_dl[, Pakka_form_ID := new_pfi]
  print(new_dl)
#  aggregate <- new_dl[, .(Count = sum(Count), MID = max(MID)), by = .(Name, Maindeck, Pakka_form_ID, Pakka_ID)]


  return(new_dl)
}

