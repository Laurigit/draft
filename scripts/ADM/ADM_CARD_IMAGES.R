#ADM_CARD_IMAGES
required_data("STG_CARDS_DIM")
images <- data.table(file_name = dir("www"))
images[, basic_land := str_sub(file_name, 1, 2) == "bl"]
images[, card_image := word(file_name, sep = fixed("_"))]

nonbasics <- suppressWarnings(images[basic_land == FALSE & is.numeric(as.numeric(card_image))])

#download missing images
current_cards <- STG_CARDS_DIM[, .(MID)]
card_images <- nonbasics[, .(MID = as.numeric(card_image), missing_image = FALSE)]
joini <- card_images[current_cards, on = "MID"][is.na(missing_image)]
for(mid_loop in joini[,MID]) {
  getCardImg_full(mid_loop)
}

ADM_CARD_IMAGES <- suppressWarnings(images[, .(file_name, basic_land, MID = as.numeric(card_image))])
