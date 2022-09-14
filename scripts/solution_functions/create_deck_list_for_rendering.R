# #render decklist
# #must be dnd
# #muset be able to drag to other Dnd objects
# #must accept filters (multiple dimensions)
# #must accept two sort orders
#
# #input: full deck, filters, sort, grouping
# #output: table where one row is a card and it cointains x,y coordinates, available filters, like colors in deck
#
#
# required_data("ADM_VISUALIZE_CARDS")
# testDeck <- ADM_VISUALIZE_CARDS[Pakka_form_ID == 518, .(Converted_Cost, Name, Rarity, Colors, Power, Toughness,
#                                                         Count, Name_count, is_basic_land, Maindeck)]
# rest <- create_deck_filters(testDeck)
# ressit <- rest[3:5, .(subset_list = list(options[[1]][1:3])), by = filter]
# total_string <- NULL
# for(koodiloop in 1:nrow(ressit)) {
#   eka_rivi <-   paste0(ressit[koodiloop, filter], ' %in% ' ,ressit[koodiloop, subset_list])
#   if(koodiloop > 1) {
#     total_string <- paste0(eka_rivi, " & ", total_string)
#   } else {
#     total_string <- paste0(eka_rivi)
#
#   }
# }
# # create_string <- paste0(lapply(ressit, function(x) {
# #   paste0(ressit[, filter], ' %in% ' ,ressit[, subset_list])
# # }), sep = "|")
# # create_string
#   syntax <- (total_string)
# filtered_deck <- testDeck[eval(parse(text = syntax))]
#
# row_dim <- "Power"
# column_sort_dim <- "Maindeck"
#
# table_to_render <- filtered_deck[order(get(row_dim), get(column_sort_dim))][,  .(x = get(row_dim), Name,
#                       y = seq_len(.N)),
#                       by =  get(row_dim)]
# table_to_render[, get := NULL]
#
#
#
