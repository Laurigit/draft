#PÄIVITÄ MSTAT ENNEN KU AJAT TÄLLÄ.
#laurinmaxPID <- SRC_CARDS[Pakka_ID %in% c(21, 22), max(Pakka_form_ID), by = Pakka_ID][, V1]
#STAT_SIDE_CARD_AGE[Pakka_ID %in% c(21)]
debug <- STAT_SIDE_CARD_AGE[Pakka_ID %in% c(21, 22), .(maxika = max(Card_age), minage = min(Card_age)), by = .( Name, Pakka_ID)]
debug[maxika != minage]
debnames <- debug[maxika != minage, Name]

remove_me <- STAT_SIDE_CARD_AGE[Pakka_ID %in% c(21, 22) & Card_age >= 10, .(Name, Pakka_ID)]#[Name == "Balduvian Horde"]
#STAT_SIDE_CARD_AGE[Name == "Angelic Renewal" & Pakka_ID %in% c(21, 22)]
#remove_me[, .N, by = Name][N >1]

DT <- remove_me[,Indx:=1:.N,by=.(Name, Pakka_ID)]
DT[Name == "Duress"]
DT[, DELME := TRUE]
DT[, Pakka_ID := as.numeric(Pakka_ID)]
required_data("SRC_CARDS")

laurinmaxPID <- SRC_CARDS[Pakka_ID %in% c(21, 22), max(Pakka_form_ID), by = Pakka_ID][, V1]
#laurija[Pakka_ID == 21]
laurija <- SRC_CARDS[Pakka_form_ID %in% laurinmaxPID]
sorted <- laurija[order(Pakka_ID, Name, DRAFT_CARDS_ID)]
sorted[, Indx := seq_len(.N), by = .(Pakka_ID, Name)]
sorted[Name == "Balduvian Horde"]
joinaa <- DT[sorted, on = .(Name, Indx, Pakka_ID)]

joinaa[Name == "Balduvian Horde"]
joinaa[Name %in% c(debnames)]

laurin_piffi <- SRC_CARDS[, max(Pakka_form_ID)] + 1
martin_piffi <- laurin_piffi + 1
laurin_aikaleima <- paste0(as.IDate(Sys.time()), " 10:00:00")
martin_aikaleima <- paste0(as.IDate(Sys.time()), " 10:00:05")
#uudet 1480, 1481
joinaa[Pakka_ID == 21, Pakka_form_ID := laurin_piffi]
joinaa[Pakka_ID == 22, Pakka_form_ID := martin_piffi]
joinaa[Pakka_ID == 21, Valid_from_DT := laurin_aikaleima]
joinaa[Pakka_ID == 22, Valid_from_DT := martin_aikaleima]
deck_left <- joinaa[is.na(DELME)]
deck_left[, ':=' (DELME = NULL, Indx = NULL)]
deck_left$Card_ID <- as.numeric(deck_left$Card_ID)
str(deck_left)
#dbWriteTable(con, "CARDS", deck_left, row.names = FALSE, append = TRUE)
joinaa[!is.na(DELME)]
