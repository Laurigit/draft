#read bulk to db

#mystery booster normals and mb foils are different deck lists

res2 <- fromJSON(txt = "./external_files/deck-19f17836-a73c-4476-a149-a83284b9e714.json")

res3 <- fromJSON(txt = "./external_files/mystery_booster_foils.json")
#include mystery booster separately
mystery_names <- c(res2$entries$columna$card_digest$name,
                   res3$entries$columnb$card_digest$name)

#HEI LAURI!
#lataa täältä https://scryfall.com/docs/api/bulk-data default cards setti

res <- fromJSON(txt = "./external_files/default-cards-20230805090459.json")
mid_name_table <- NULL
counter <- 0
for(loopJson in 1:nrow(res)) {
counter <- counter + 1
if (counter %% 1000 == 0) {
print(counter)
}
#casting_cost <- res[[14]][2000]
#setti <- res[['set']][2000]

#{"multiverse_ids":[126015]"name":"Battering Sliver",,"mana_cost":"{5}{R}","cmc":6.0,"type_line":"Creature — Sliver","oracle_text":"All Sliver creatures have trample.","power":"4","toughness":"4","colors":["R"],

#  loopJson <- 20000

mid <- tryCatch({

  result <- data.table(MID = res[['multiverse_ids']][loopJson][[1]],
                       Name =  res[['name']][loopJson],
                       Cost = res[['mana_cost']][loopJson],
                       Converted_Cost =  res[['cmc']][loopJson],
                       Type = res[['type_line']][loopJson],
                       Text =  res[['oracle_text']][loopJson],
                       Stats = paste0(res[['power']][loopJson], "/", res[['toughness']][loopJson]),
                       Colors = paste(unlist(res[['colors']][loopJson]), collapse = ""),
                       Set = res[['set']][loopJson],
                       Rarity = res[['rarity']][loopJson])

  # if (result[, Name] == "Resurgent Belief") {
  #   browser()
  # }
  if ((result[, Set] %in% c("mma", "mm2", "mm3", "uma", "ema", "m19", "m20", "ima", "mh1", "a25", "mh2", "2xm", "2x2", "m21", "dmr", "tsr", "dtk", "znr") |
      result[, Name %in% mystery_names]) & !is.na(result[, MID]))  {
   # if (is.na(result[, MID]) == TRUE) {browser()}

  mid_name_table <- rbind(result, mid_name_table)
  }
}, error =  function(e) {

  data.table(MID = "errori")
})


# -
# if (mid[, MID] != "errori") {
#   mid_name_table <- rbind(mid, mid_name_table)
#   print(mid[, MID])
# }
}
aggr <- mid_name_table[, .(MID = max(MID)), by = .(Name, Cost, Converted_Cost, Type, Text, Stats, Colors, Rarity, Set)]#[MID != 526748]
#aggr[ str_detect(Type, "—")]
#utf8ToInt("-")
#aggr[ str_detect(Type, "—")]
#aggr[, Type2 := str_replace(Type, "—", "-")]
#aggr <- aggr[ str_detect(Type, "Land — Urza")]
#str(aggr)

aggr[, ':=' (#Name = iconv(x = Name, to = "UTF-8"),

             Text = iconv(x = Text, to = "UTF-8"))]
aggr[, ':=' (#Name = iconv(x = Name, to = "UTF-8"),

  Type = iconv(x = Type, to = "UTF-8"))]
#aggr[, Type :=  gsub("—", "!", Type)]
aggr[, Card_ID := ""]
con <- connDB(con)

dbSendQuery(con, 'SET NAMES utf8')
#dbWriteTable(con, "CARDS_DIM_with_set", aggr, row.names = FALSE, append = TRUE, overwrite = FALSE)
#OTA SET POIS ENNEN KU LISÄÄT UUSIA KORTTEJA OIKEESEEN DATAAN
#aggr[, Set := NULL]
#dbWriteTable(con, "CARDS_DIM", aggr, row.names = FALSE, append = TRUE, overwrite = FALSE)
#dbWriteTable(con, "delme_CARDS_DIM_delme", aggr, row.names = FALSE,  overwrite =TRUE)
