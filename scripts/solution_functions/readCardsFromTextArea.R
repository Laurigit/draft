readCardsFromTextArea <- function(textAreaInput_text, con, STG_CARDS_DIM) {


#   textAreaInput_text <- "Silverflame Squire // On Alert; 0
# Ardenvale Tactician // Dizzying Swoop; 0
# Curious Pjklhjlair // Treats to Share; 0
# Giant Opportunity; 473121
# Oakhame Ranger // Bring Back; 0
# Gogin Guide; 425921"



  taulu_all <- data.table(read.csv(text = textAreaInput_text, sep = ";", dec = ",", header = FALSE))
  setnames(taulu_all, c("V1", "V2"), c("Name", "MID"))

 #  #fix missing MIDs
 # # tulos <- getCard_from_SF("Aethersnipe")[,MID]
 #  try_res <- tryCatch({
 #    taulu[MID == 0, MID := getCard_from_SF(Name, STG_CARDS_DIM)[,MID], by = Name]
 #  }, error = function(e) {
 #    "ERROR"
 #  })

  #find the mid in the DB
  sscols_cards_dim <- STG_CARDS_DIM[, .(Name, MID)]
  taulu_all[, MID := NULL]
  taulu <- sscols_cards_dim[taulu_all, on = "Name"]


  more_info <- dbQ(paste0("SELECT Name, Converted_Cost, Colors from CARDS_DIM"),  con)
  join_more <- more_info[taulu, on = "Name"]

return(join_more)


}
