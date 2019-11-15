count_deck_stats <- function(input_PFI, STG_CARDS, STG_CARDS_DIM) {
  #input_PFI <- 518 STG_CARDS[Pakka_ID == 1 & Pakka_form_ID==518]
  #input_PFI <- 520
  dl <- STG_CARDS[Pakka_form_ID == input_PFI & Maindeck == 1, .(Name)]
  card_info <- STG_CARDS_DIM[dl, on = "Name"]
  card_info[, is_land := grepl(x = Type, pattern = "Land")]
  res <- NULL
  res$land_count <- card_info[is_land == TRUE, .N]
  res$card_count <- card_info[, .N]
  return(res)
  }
