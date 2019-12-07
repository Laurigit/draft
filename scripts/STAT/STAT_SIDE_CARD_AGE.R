#STAT_SIDE_CARD_AGE.R
con <- connDB(con)
STAT_SIDE_CARD_AGE <- dbSelectAll("STAT_SIDE_CARD_AGE", con)
