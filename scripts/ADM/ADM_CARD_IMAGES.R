#ADM_CARD_IMAGES
required_data("STG_CARDS_DIM")
images <- data.table(file_name = dir("www"))
images[, basic_land := str_sub(file_name, 1, 2) == "bl"]
images[, card_image := word(file_name, sep = fixed("_"))]
images[, small_card := word(file_name, 3, 3, sep = fixed("_")) == "small.jpg"]
nonbasics <- suppressWarnings(images[basic_land == FALSE & is.numeric(as.numeric(card_image))])
nonbasics[, count_of_images := .N, by = card_image]
#download missing images
current_cards <- STG_CARDS_DIM[, .(MID)]
#card_images <- nonbasics[, .(MID = as.numeric(card_image), missing_image = FALSE)]
card_images <- nonbasics[count_of_images == 2,  .(missing_image = FALSE), by = (MID = as.numeric(card_image))]

joini <- card_images[current_cards, on = "MID"][is.na(missing_image)]
for(mid_loop in joini[,MID]) {
  getCardImg_full(mid_loop)
}

ADM_CARD_IMAGES <- suppressWarnings(images[, .(file_name, basic_land, MID = as.numeric(card_image))])
