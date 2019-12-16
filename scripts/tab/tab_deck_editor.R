
table_to_render_react <- reactive({


  testDeck <- ADM_VISUALIZE_CARDS[Pakka_form_ID == input$choose_decklist, .(Converted_Cost, Name, Rarity, Colors, Power, Toughness,
                                                                            Count, Name_count, is_basic_land, Maindeck)]
  rest <- create_deck_filters(testDeck)
  ressit <- rest[1, .(subset_list = list(options[[1]][1:5])), by = filter]
  total_string <- NULL
  for(koodiloop in 1:nrow(ressit)) {
    eka_rivi <-   paste0(ressit[koodiloop, filter], ' %in% ' ,ressit[koodiloop, subset_list])
    if(koodiloop > 1) {
      total_string <- paste0(eka_rivi, " & ", total_string)
    } else {
      total_string <- paste0(eka_rivi)
    }
  }
  # create_string <- paste0(lapply(ressit, function(x) {
  #   paste0(ressit[, filter], ' %in% ' ,ressit[, subset_list])
  # }), sep = "|")
  # create_string
  syntax <- (total_string)
  filtered_deck <- testDeck[eval(parse(text = syntax))]

  row_dim <- "Power"
  column_sort_dim <- "Maindeck"

  table_to_render <- filtered_deck[order(get(row_dim), get(column_sort_dim))][,  .(x = get(row_dim), Name,
                                                                                   y = seq_len(.N)),
                                                                              by =  get(row_dim)]
  table_to_render[, get := NULL]
  table_to_render
})

#tab_deck_editor
output$decklist <- renderUI({

  table_to_render <- table_to_render_react()
  required_data("STG_CARDS_DIM")
  sscols_cards <- STG_CARDS_DIM[, .(MID, Name)]

  fluidPage(
    # for (sarake in 1:table_to_render[, max(x)]) {
    #
    # }
    lapply(table_to_render[, unique(x)], function(rivi) {

      column(width = 1,
             offset = 0,
             lapply(table_to_render[x == rivi, unique(y)], function(sarake) {
               kuva_id <- table_to_render[x == rivi & y == sarake, paste0(x, Name, y)]
               card_name <-  table_to_render[x == rivi & y == sarake, Name]
               MIDi <- sscols_cards[Name == card_name, MID]
               getCardImg_full(MIDi)
               output[[kuva_id]] <-  renderImage({

                 # output[[image_id]] <-  renderImage({
                 list(src = paste0("./www/", MIDi, "_card.jpg"),#image_nm,
                      alt = "Image failed to render"
                 )
               }, deleteFile = FALSE)
               imageOutput(kuva_id,
                           height = "100px")
             })
      )
    })

  )
})

