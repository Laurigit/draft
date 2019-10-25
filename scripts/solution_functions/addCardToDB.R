addCardToDB <- function(MID, con) {
 res <-  getCard_by_MID(MID)
#MID <- 394550
 getCardImg_full(MID)
  dbIoU("CARDS_DIM", res, con)
}


