count_deck_stats <- function(input_PFI, STG_CARDS, STG_CARDS_DIM) {
  #input_PFI <- 518 #STG_CARDS[Pakka_ID == 1 & Pakka_form_ID==518]
  #input_PFI <- 520
  dl <- STG_CARDS[Pakka_form_ID == input_PFI & Maindeck == 1, .(Name)]
  card_info <- STG_CARDS_DIM[dl, on = "Name"]
  card_info[, is_land := grepl(x = Type, pattern = "Land")]
  card_info[, is_creature := grepl(x = Type, pattern = "Creature")]
  card_info[, is_enchantment := grepl(x = Type, pattern = "Enchantment")]
  card_info[, is_sorcery := grepl(x = Type, pattern = "Sorcery")]
  card_info[, is_instant := grepl(x = Type, pattern = "Instant")]
  card_info[, is_artifact := grepl(x = Type, pattern = "Artifact")]
  mana_string <- card_info[, .(W = sum(W), B = sum(B), G = sum(G), R = sum(R), U = sum(U))]
  mana_string[, all := W + B + G + R + U]
  ms <- mana_string[, paste0(ifelse(W > 0, paste0("W", round(W / all * 100), " "), ""),
                       ifelse(U > 0, paste0("U", round(U / all * 100), " "), ""),
                       ifelse(R > 0, paste0("R", round(R / all * 100), " "), ""),
                       ifelse(B > 0, paste0("B", round(B / all * 100), " "), ""),
                       ifelse(G > 0, paste0("G", round(G / all * 100), " "), ""))]
  res <- NULL
  res$mana_costs <- ms
  res$creature_count <- card_info[is_creature == TRUE, .N]
  res$inc_or_sorc_count <- card_info[is_instant == TRUE, .N] + card_info[is_sorcery == TRUE, .N]
  res$land_count <- card_info[is_land == TRUE, .N]
  res$card_count <- card_info[, .N]
  return(res)
  }
