#cardNameInput <- "Kitchen Finks"
#getCardImg_full(cardNameInput)
getCardImg_full <- function(cardMID) {

  #check if exists
  #fixedName <- stringi::stri_trans_general(cardNameInput, "Latin-ASCII")
  if (!file.exists(paste0("../common_data/", cardMID, "_card.jpg"))) {


    #cardNameInput <- "Vampire Aristocrat"

    url <- paste0("https://api.scryfall.com/cards/multiverse/", cardMID)
    raw.result <- GET(url = url)
    result_json <- fromJSON(rawToChar(raw.result$content))

      image_url <- result_json$image_uris$small



    download.file(url = image_url, destfile = paste0("../common_data/", cardMID, "_card.jpg"), mode = "wb")
  }
  if (!file.exists(paste0("./www/", cardMID, "_card.jpg"))) {
    file.copy(from = paste0("../common_data/", cardMID, "_card.jpg"),
              to = paste0("./www/", cardMID, "_card.jpg"))
    file.copy(from = paste0("./www/", cardMID,"_card.jpg"),
              to = paste0("./www/", cardMID, "_card.jpg"))
  }
}
