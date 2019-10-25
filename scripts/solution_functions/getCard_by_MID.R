
getCard_by_MID <- function(cardMID) {
  #cardMID <- "394553"
  url <- paste0("https://api.scryfall.com/cards/multiverse/", cardMID)

  raw.result <- GET(url = url)
  result_json <- fromJSON(rawToChar(raw.result$content))

  result_row <- data.table( MID = cardMID,
                            Name = gsub("[[:punct:]]", "", result_json$name),
                            Text = gsub("[[:punct:]]", "", ifelse(is.null(result_json$oracle_text), NA,  result_json$oracle_text)),
                            Cost = ifelse(is.null(result_json$mana_cost), NA, result_json$mana_cost),
                            Converted_Cost = ifelse(is.null(result_json$cmc), NA, result_json$cmc),
                            Rarity = result_json$rarity,
                            Colors = ifelse(is.null(result_json$colors), NA, result_json$colors),
                            Stats = paste0(ifelse(is.null(result_json$power), NA, result_json$power),
                                           "/",
                                           ifelse(is.null(result_json$toughness), NA, result_json$toughness)))


  return(result_row)
}
