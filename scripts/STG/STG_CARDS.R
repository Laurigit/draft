#STG_CARDS
required_data("SRC_CARDS")
STG_CARDS <- SRC_CARDS[, .(MID = as.numeric(MID),
                           Pakka_ID,
                           Pakka_form_ID = as.numeric(Pakka_form_ID),
                           Card_ID = as.numeric(Card_ID),
                           DRAFT_CARDS_ID = as.numeric(DRAFT_CARDS_ID),
                           Name,
                           Maindeck)]

