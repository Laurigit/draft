addCardToDB <- function(Name, con) {

 res <-  getCard_from_SF(Name)


  dbIoU("CARDS_DIM", res, con)
}


