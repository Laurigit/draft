readCardsFromTextArea <- function(textAreaInput_text, con, STG_CARDS_DIM) {


#   textAreaInput_text <- "Silverflame Squire // On Alert; 0
# Ardenvale Tactician // Dizzying Swoop; 0
# Curious Pjklhjlair // Treats to Share; 0
# Giant Opportunity; 473121
# Oakhame Ranger // Bring Back; 0
# Gogin Guide; 425921"
  test_text  <- "
  Abbot of Keral Keep; 0; 2022-11-18_09:59:08
  Abiding Grace; 0; 2022-11-18_09:58:54
Abnormal Endurance; 0; 2022-11-18_09:58:40
Abominable Treefolk; 0; 2022-11-18_09:59:06
Goblin Token; 0; 2022-11-18_09:59:05
Abrade; 0; 2022-11-18_09:58:13
Abrupt Decay; 0; 2022-11-18_09:57:18
Absorb; 0; 2022-11-18_09:58:25
Absorb Vis; 0; 2022-11-18_09:58:44
Abundant Growth; 0; 2022-11-18_09:59:02
Abundant Harvest; 0; 2022-11-18_09:58:46
Abyssal Persecutor; 0; 2022-11-18_09:59:00"

 # taulu_all <- data.table(read.csv(text = test_text, sep = ";", dec = ",", header = FALSE))
  #taulu_all[, Name := trimws(Name)]
  taulu_all <- data.table(read.csv(text = textAreaInput_text, sep = ";", dec = ",", header = FALSE))
  setnames(taulu_all, c("V1", "V2", "V3"), c("Name", "MID", "import_date"))
taulu_all[, sort_date := strptime(import_date, "%Y-%m-%d_%H:%M:%S")]
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
  sorttaa <- join_more[order(sort_date)]
  sorttaa[, ':=' (import_date = NULL, sort_date = NULL)]
return(sorttaa)


}
