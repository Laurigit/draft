readCardsFromTextArea <- function(textAreaInput_text, con) {


#   textAreaInput_text <- "Silverflame Squire // On Alert; 0
# Ardenvale Tactician // Dizzying Swoop; 0
# Curious Pjklhjlair // Treats to Share; 0
# Giant Opportunity; 473121
# Oakhame Ranger // Bring Back; 0
# Gogin Guide; 425921"

  taulu <- data.table(read.csv(text = textAreaInput_text, sep = ";", dec = ",", header = FALSE))
  setnames(taulu, c("V1", "V2"), c("Name", "MID"))

  #fix missing MIDs
 # tulos <- getCard_from_SF("Aethersnipe")[,MID]
  try_res <- tryCatch({
    taulu[MID == 0, MID := getCard_from_SF(Name)[,MID], by = Name]
  }, error = function(e) {
    "ERROR"
  })


if (try_res != "ERROR") {
  MIDs <- dbQ(paste0("SELECT Name from CARDS_DIM"),  con)
  new_cards <- setdiff(taulu[, Name], MIDs[, Name])
  for (x in new_cards) {
    res <- getCard_from_SF(x)
    dbIoU("CARDS_DIM", res, con)

  }
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_CARDS", ADM_DI_HIERARKIA, globalenv())
  more_info <- dbQ(paste0("SELECT Name, Converted_Cost, Colors from CARDS_DIM"),  con)
  join_more <- more_info[taulu, on = "Name"]

return(join_more)
} else {
  return(try_res)
}
}
