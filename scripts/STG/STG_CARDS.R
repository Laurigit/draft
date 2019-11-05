#STG_CARDS
required_data("SRC_CARDS")
temp <- SRC_CARDS[, .(MID = as.numeric(MID),
                           Pakka_ID,
                           Pakka_form_ID = as.numeric(Pakka_form_ID),
                           Card_ID = as.numeric(Card_ID),
                           DRAFT_CARDS_ID = as.numeric(DRAFT_CARDS_ID),
                           Name,
                           Maindeck)]


#calc running number per MID, PIF
temp[, monesko_kortti := seq_len(.N), by = .(MID, Pakka_form_ID)]
STG_CARDS <- temp
