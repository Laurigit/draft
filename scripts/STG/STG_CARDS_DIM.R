#STG_CARDS_DIM
required_data("SRC_CARDS_DIM")
temp <- suppressWarnings(SRC_CARDS_DIM[, .(MID = as.numeric(MID),
                                   #Card_ID = as.numeric(Card_ID),
                                   Name = Name,iconv(x = Name, to = "UTF-8"),
                                   Text,
                                   Cost = Cost,
                                   Converted_Cost = as.numeric(Converted_Cost),
                                   Rarity,
                                   Colors,
                                   Stats,
                                   Power = as.numeric(word(Stats, 1, 1, sep = "/")),
                                   Toughness = as.numeric(word(Stats, 2, 2, sep = "/")),
                                   Type = gsub("—", "-", gsub("â€”", "-", Type))), by = MID])

temp[, Cost_clean := gsub("}", "", Cost)]
temp[, Cost_clean := gsub("\\{", "", Cost_clean)]
all_chars <- paste0(temp[, Cost_clean], collapse = "")
uniqchars <- data.table(kirjaimet = strsplit(all_chars, "")[[1]])[, .N, by = kirjaimet]
uniqchars[, isnum := !is.na(as.numeric(kirjaimet))]
manacost_sarakkeet  <- uniqchars[isnum == FALSE  & !kirjaimet %in% c("/", "A", "N"), kirjaimet]

lapply(manacost_sarakkeet, function(inp) {
  temp[, (inp) := str_count(Cost,inp)]

})
temp[, AnyColor := as.numeric((gsub("([0-9]+).*$", "\\1", Cost_clean)))]
temp[, AnyColor := ifelse(is.na(AnyColor), 0, AnyColor)]


#fix land MIDs

temp[, ':=' (
  Type_exact = word(Type, start = 1, sep = " - "),
  Tribe_total = word(Type, start = 2, sep =" - ")
)]
suppressWarnings(temp[, ':=' (Race = word(Tribe_total, start = 1, sep = " "),
                     Class = word(Tribe_total, start = 2, sep = " "),
                     Subclass = word(Tribe_total, start = 3, sep = " "),
                     Subtype = word(Type_exact, start = -2, end = -2, sep = " "))])

landitaulu <- data.table(Name = c("Forest","Swamp","Plains",                                  "Mountain",
                                  "Island",
                                  "Wastes")
                         ,  land_MID = c(289327,
                                         473220,
                                         473212,
                                         221305,
                                         386333,
                                         407693))
joinlandi <- landitaulu[temp, on = "Name"]
joinlandi[, MID := ifelse(is.na(land_MID), MID, land_MID)]
joinlandi[, land_MID := NULL]
STG_CARDS_DIM <- joinlandi


#join_ss_aggr[, ':=' (
#Type_exact = word(Type, start = 1, sep = " — "),
#Tribe_total = word(Type, start = 2, sep =" — ")
#)]
#join_ss_aggr[, ':=' (Race = word(Tribe_total, start = 1, sep = " "),
#                     Class = word(Tribe_total, start = 2, sep = " "),
#                     Subclass = word(Tribe_total, start = 3, sep = " "),
#                     Subtype = word(Type_exact, start = -2, end = -2, sep = " "))]
