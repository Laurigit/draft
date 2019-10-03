addCardToDB <- function(MID, con) {
 res <-  getCard_by_MID(MID)
 getCardImg_full(MID)
  dbIoU("CARDS_DIM", res, con)
}


