#cardNameInput <- "Kitchen Finks"
#getCardImg_full(cardNameInput)

small_or_normal <- function(card_post_fix, cardMID) {

  if (!file.exists(paste0("../common_data/", cardMID, card_post_fix))) {


    #cardNameInput <- "Vampire Aristocrat"
#cardMID <- 152648
   # card_post_fix <-"_card_small.jpg"
    url <- paste0("https://api.scryfall.com/cards/multiverse/", cardMID)
    raw.result <- GET(url = url)
    result_json <- fromJSON(rawToChar(raw.result$content))

    if (card_post_fix == "_card_small.jpg") {
      image_url <- result_json$image_uris$small
    } else {
      image_url <- result_json$image_uris$normal
    }


    download.file(url = image_url, destfile = paste0("../common_data/", cardMID, card_post_fix), mode = "wb")
  }
  if (!file.exists(paste0("./www/", cardMID, card_post_fix))) {
    file.copy(from = paste0("../common_data/", cardMID, card_post_fix),
              to = paste0("./www/", cardMID, card_post_fix))
    file.copy(from = paste0("./www/", cardMID,card_post_fix),
              to = paste0("./www/", cardMID, card_post_fix))
  }
}
getCardImg_full <- function(cardMID) {

  #check if exists
  #cardMID <- 152648
  #fixedName <- stringi::stri_trans_general(cardNameInput, "Latin-ASCII")

 small_or_normal("_card_small.jpg", cardMID)
 small_or_normal("_card.jpg", cardMID)
}

