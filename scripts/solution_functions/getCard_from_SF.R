
getCard_from_SF <- function(input_card_name) {

  #check if exists
  #fixedName <- stringi::stri_trans_general(cardNameInput, "Latin-ASCII")

  # cardNameInput <- "Æthersnipe"
  #cardNameInput <- "Kongming,+\"Sleeping+Dragon\""
  urlName_frst <- gsub(" ", "+", x = input_card_name)
  urlName <- gsub("\"", "", x = urlName_frst)
  url <- paste0('https://api.scryfall.com/cards/named?exact=\'',  urlName, '\'')
  raw.result <- GET(url = url)
  result_json <- fromJSON(rawToChar(raw.result$content))
  mid <- result_json$multiverse_ids



  result_row <- data.table(
    Name = iconv(x = result_json$name, to = "UTF-8"), #Name = gsub("[[:punct:]]", "", result_json$name),
    Text = iconv(x = ifelse(is.null(result_json$oracle_text), NA,  result_json$oracle_text), to = "UTF-8"),
    Cost = ifelse(is.null(result_json$mana_cost), NA, result_json$mana_cost),
    Converted_Cost = ifelse(is.null(result_json$cmc), NA, result_json$cmc),
    Rarity = result_json$rarity,
    Colors = ifelse(is.null(result_json$colors), NA, result_json$colors),
    Stats = paste0(ifelse(is.null(result_json$power), NA, result_json$power),
                   "/",
                   ifelse(is.null(result_json$toughness), NA, result_json$toughness)),
    MID = result_json$multiverse_ids)


  return(result_row)
}
