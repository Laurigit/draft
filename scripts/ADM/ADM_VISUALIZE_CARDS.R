#ADM_VISUALIZE_CARDS
required_data(c("STG_CARDS_DIM", "STG_CARDS", "ADM_LAND_IMAGES", "STG_DECKS_DIM"))

sscols_DIM <- STG_CARDS_DIM

#ota vaan max pakka_If
 max_PID <- STG_CARDS[, .(Pakka_form_ID = max(Pakka_form_ID)), by = .(Pakka_ID)]
#   sscols <- STG_CARDS[Pakka_form_ID == 555, .(Pakka_form_ID, MID, DRAFT_CARDS_ID, Maindeck, Pakka_ID)]
sscols <- STG_CARDS[Pakka_form_ID %in% max_PID[, Pakka_form_ID], .(Pakka_form_ID, MID, DRAFT_CARDS_ID, Maindeck, Pakka_ID)]

joini_all <- sscols_DIM[sscols, on = "MID", allow.cartesian = TRUE]

print("TOIMI")
#sscolsDecks <- STG_DECKS_DIM[Omistaja_NM == session$user, .(NineSide, Pakka_ID)]
sscolsDecks <- STG_DECKS_DIM[, .(NineSide, Pakka_ID)]
print("EITOIMIs")
joini <- joini_all[sscolsDecks, on = .(Pakka_ID)]



joini[, colOrder := Converted_Cost]
#basic_land_types <- joini[Rarity == "Basic Land", .(Basic_land_types = uniqueN(Name)), by = Pakka_form_ID]
#joinaa
#joinBasic <- joini[basic_land_types, on = "Pakka_form_ID"]

#are there non-basic_lands
joini[is.na(Converted_Cost) , colOrder := -1]

#levita non-basic-land kortit. Aggregoi basic land kortit
joini[, is_basic_land := ifelse(Name %in% c("Mountain", "Island", "Forest", "Wastes", "Swamp", "Plains"), TRUE, FALSE)]
basics <- joini[is_basic_land == TRUE, .(Count = .N, DRAFT_CARDS_ID = max(DRAFT_CARDS_ID)), by = .(Name, Text, Cost, Converted_Cost, Rarity, Colors, Stats,
                                                             Pakka_form_ID, Maindeck, Pakka_ID, colOrder, is_basic_land,
                                                             NineSide)]

#joinaa image id ja MID
join_image_id_to_lands <- ADM_LAND_IMAGES[basics, on = .(Count, Name)]

levita_muut <- joini[is_basic_land == FALSE, .(Name, Text, Cost, Converted_Cost, Rarity, Colors, Stats, DRAFT_CARDS_ID,
                                         Pakka_form_ID, Maindeck, Count = 1, Pakka_ID, colOrder, MID, is_basic_land,
                                        NineSide)]



levita_muut[, image_file := paste0(MID, "_card_small.jpg")]

#appendaa
levita_data <-  rbind(levita_muut, join_image_id_to_lands, fill = TRUE)
levita_data[, image_id := paste0("img_", DRAFT_CARDS_ID)]
#levita_data[, image_id := paste0("img", seq_len(.N))]
#levita_data[, image_id := DRAFT_CARDS_ID]
#lisataan riveja countin mukaan, jotta osataan piirtaa oikee maara kortteja


ADM_VISUALIZE_CARDS <- levita_data#[Pakka_form_ID > 475]#[sample(nrow(levita_data))[1:3500]]
#ADM_VISUALIZE_CARDS[is.na(Converted_Cost)]
