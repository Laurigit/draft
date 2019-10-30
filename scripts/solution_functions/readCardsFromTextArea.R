readCardsFromTextArea <- function(textAreaInput_text, con) {


  leike_all <- as.numeric(str_split(gsub("[\r\n]", "/", textAreaInput_text), pattern = "/")[[1]])
  leike <- leike_all[!is.na(leike_all)]
  MIDs <- dbQ(paste0("SELECT MID from CARDS_DIM"),  con)
  new_cards <- setdiff(leike, MIDs[, MID])
  for (x in new_cards) {
    addCardToDB(x, con)

  }
return(leike)
}
