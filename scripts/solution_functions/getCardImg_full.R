#cardNameInput <- "Kitchen Finks"
#getCardImg_full(cardNameInput)
getCardImg_full <- function(cardNameInput) {
  #check if exists
  fixedName <- stringi::stri_trans_general(cardNameInput, "Latin-ASCII")
  if (!file.exists(paste0("../common_data/", cardNameInput, "_card.jpg"))) {


    #cardNameInput <- "Vampire Aristocrat"
    urlName <- gsub(" ", "+", x = cardNameInput)
    url <- paste0("https://api.scryfall.com/cards/named?exact=", urlName)
    raw.result <- GET(url = url)
    result_json <- fromJSON(rawToChar(raw.result$content))

      image_url <- result_json$image_uris$small



    download.file(url = image_url, destfile = paste0("../common_data/", cardNameInput, "_card.jpg"), mode = "wb")
  }
  if (!file.exists(paste0("./www/", fixedName, "_card.jpg"))) {
    file.copy(from = paste0("../common_data/", cardNameInput, "_card.jpg"),
              to = paste0("./www/", cardNameInput, "_card.jpg"))
    file.copy(from = paste0("./www/", cardNameInput,"_card.jpg"),
              to = paste0("./www/", fixedName, "_card.jpg"))
  }
}
