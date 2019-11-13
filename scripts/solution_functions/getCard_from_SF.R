
getCard_from_SF <- function(input_card_name) {

  #check if exists
  #fixedName <- stringi::stri_trans_general(cardNameInput, "Latin-ASCII")

  # input_card_name <- "Aethersnipe"
  # input_card_name <- "Lightning Helix"
  #input_card_name <- "Kongming,+\"Sleeping+Dragon\""
  urlName_frst <- gsub(" ", "+", x = input_card_name)
  urlName <- gsub("\"", "", x = urlName_frst)
  url <- paste0('https://api.scryfall.com/cards/named?exact=\'',  urlName, '\'')
  raw.result <- GET(url = url)

  raw_iconv  <- content(raw.result, "text", encoding = "UTF-8")
  result_json <- fromJSON(raw_iconv)
  mid <- result_json$multiverse_ids
  if (!is.numeric(mid)) {
    #backupMID
    #https://api.magicthegathering.io/v1/cards?name=%22Goblin%20Guide%22
    # urli_bu <- paste0('https://api.magicthegathering.io/v1/cards?name=\"',  input_card_name, '\"')

    urli_bu <- paste0('https://api.magicthegathering.io/v1/cards?name=',  urlName)
    raw.result_bu1 <- GET(url = urli_bu)
    result_json_bu <- fromJSON(rawToChar(raw.result_bu1$content))
    mid <- max(result_json_bu$cards$multiverseid, na.rm = TRUE)

  }
  if (is.null(result_json$colors)) {
    colorPaste <- NA
  } else {
    colorFix <- unlist(result_json$colors)
    colorPaste <- paste0(colorFix, collapse = "")
  }
  result_row <- data.table(
    Name = iconv(x = result_json$name, to = "UTF-8"), #Name = gsub("[[:punct:]]", "", result_json$name),
    Text = iconv(x = ifelse(is.null(result_json$oracle_text), NA,  result_json$oracle_text), to = "UTF-8"),
    Cost = ifelse(is.null(result_json$mana_cost), NA, result_json$mana_cost),
    Type = iconv(x = result_json$type_line, to = "UTF-8"),
    Converted_Cost = ifelse(is.null(result_json$cmc), NA, result_json$cmc),
    Rarity = result_json$rarity,
    Colors = colorPaste,
    Stats = paste0(ifelse(is.null(result_json$power), NA, result_json$power),
                   "/",
                   ifelse(is.null(result_json$toughness), NA, result_json$toughness)),
    MID = mid)


  return(result_row)
}
