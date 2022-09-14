addCardToDB <- function(Name, con, STG_CARDS_DIM) {

 res <-  getCard_from_SF(Name, STG_CARDS_DIM)


  dbIoU("CARDS_DIM", res, con)
}


