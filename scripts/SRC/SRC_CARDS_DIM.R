#SRC_CARDS_DIM
con <- connDB(con)
SRC_CARDS_DIM <- dbSelectAll("CARDS_DIM", con)
