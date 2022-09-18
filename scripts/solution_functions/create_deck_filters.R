#create filter from decklist
#input: full deck
#output: available filters, like colors in deck


#required_data("ADM_VISUALIZE_CARDS")
#testDeck <- ADM_VISUALIZE_CARDS[Pakka_form_ID == 518, .(Converted_Cost, Name, Rarity, Colors, Power, Toughness,
#                                                        Count, Name_count, is_basic_land)]
#rest <- create_deck_filters(testDeck)
create_deck_filters <- function(input_dl, remove_extra = TRUE) {
filter_table <- NULL
filterCols <- colnames(input_dl)
for (filter in filterCols) {
  #loop_res <- testDeck[, (.N), by = filter][, filter]
  uniik <- sort(input_dl[, unique(get(filter))])
  row_data <- data.table(filter, options = list(uniik))
  filter_table <- rbind(filter_table, row_data)
}
if (remove_extra == TRUE) {
  result <- filter_table[!filter %in% c("Name", "image_id", "DRAFT_CARDS_ID", "image_id_new")]
} else {
  result <- filter_table
}
return(result)
}
