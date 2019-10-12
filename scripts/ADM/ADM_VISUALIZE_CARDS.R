#ADM_VISUALIZE_CARDS
required_data(c("STG_CARDS_DIM", "STG_CARDS", "ADM_LAND_IMAGES", "STG_DECKS_DIM"))

sscols_DIM <- STG_CARDS_DIM
sscols <- STG_CARDS[, .(Pakka_form_ID, MID, Count, Maindeck, Pakka_ID)]

joini_all <- sscols_DIM[sscols, on = "MID"]

#join nineside info
sscolsDecks <- STG_DECKS_DIM[, .(NineSide, Pakka_ID)]
joini <- sscolsDecks[joini_all, on = .(Pakka_ID)]



joini[, colOrder := Converted_Cost]
#basic_land_types <- joini[Rarity == "Basic Land", .(Basic_land_types = uniqueN(Name)), by = Pakka_form_ID]
#joinaa
#joinBasic <- joini[basic_land_types, on = "Pakka_form_ID"]

#are there non-basic_lands
joini[is.na(Converted_Cost) , colOrder := -1]

#levita non-basic-land kortit. Aggregoi basic land kortit
joini[, is_basic_land := ifelse(Name %in% c("Mountain", "Island", "Forest", "Wastes", "Swamp", "Plains"), TRUE, FALSE)]
basics <- joini[is_basic_land == TRUE, .(Count = sum(Count)), by = .(Name, Text, Cost, Converted_Cost, Rarity, Colors, Stats,
                                                             Pakka_form_ID, Maindeck, Pakka_ID, colOrder, is_basic_land,
                                                             NineSide)]

#joinaa image id ja MID
join_image_id_to_lands <- ADM_LAND_IMAGES[basics, on = .(Count, Name)]

muut <- joini[is_basic_land == FALSE, .(Name, Text, Cost, Converted_Cost, Rarity, Colors, Stats,
                                         Pakka_form_ID, Maindeck, Pakka_ID, colOrder, Count, MID, is_basic_land,
                                        NineSide)]


levita_muut <- muut[rep(seq_len(nrow(muut)), Count), ]
levita_muut[, image_file := paste0(MID, "_card.jpg")]

#appendaa
levita_data <-  rbind(levita_muut, join_image_id_to_lands, fill = TRUE)
levita_data[, image_id := paste0("img", seq_len(.N))]
#lisataan riveja countin mukaan, jotta osataan piirtaa oikee maara kortteja

ADM_VISUALIZE_CARDS <- levita_data[]
#ADM_VISUALIZE_CARDS[is.na(Converted_Cost)]
