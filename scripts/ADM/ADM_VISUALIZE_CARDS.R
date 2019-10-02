#ADM_VISUALIZE_CARDS
required_data(c("STG_CARDS_DIM", "STG_CARDS"))

sscols_DIM <- STG_CARDS_DIM
sscols <- STG_CARDS[, .(Pakka_form_ID, Card_ID, Count, Maindeck)]

joini <- sscols_DIM[sscols, on = "Card_ID"]

joini[, colOrder := Converted_Cost]
#basic_land_types <- joini[Rarity == "Basic Land", .(Basic_land_types = uniqueN(Name)), by = Pakka_form_ID]
#joinaa
#joinBasic <- joini[basic_land_types, on = "Pakka_form_ID"]

#are there non-basic_lands
joini[is.na(Converted_Cost) & Rarity != "Basic Land", colOrder := -1]

#join_count_nonbasic
#join_non_basic <-  joinBasic[nonBasics, on = "Pakka_form_ID"]

#count max colo_order
#join_non_basic[, maxColOrder := max(colOrder, na.rm = TRUE), by = Pakka_form_ID]

#lisataan riveja countin mukaan, jotta osataan piirtaa oikee maara kortteja
levita_data <- joini[rep(seq_len(nrow(joini)), Count), ]
#joini[]
levita_data[, image_id := paste0("img", seq_len(.N))]
levita_data[, is_basic_land := ifelse(Name %in% c("Mountain", "Island", "Forest", "Wastes", "Swamp", "Plains"), TRUE, FALSE)]
ADM_VISUALIZE_CARDS <- levita_data
#ADM_VISUALIZE_CARDS[is.na(Converted_Cost)]
