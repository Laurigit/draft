#STG_CARDS
required_data("SRC_CARDS")
STG_CARDS <- SRC_CARDS[, .(Pakka_form_ID = as.numeric(Pakka_form_ID),
                           Card_ID = as.numeric(Card_ID),
                           Count = as.numeric(Count),
                           Name,
                           Maindeck = ifelse(Maindeck == " TRUE", TRUE, FALSE))]

