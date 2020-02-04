
output$filters <- renderUI({
  req(input$choose_decklist)
  input$choose_decklist

  testDeck <- ADM_VISUALIZE_CARDS[Pakka_ID == input$choose_decklist, .(Converted_Cost, Name, Rarity, Colors, Power, Toughness,
                                                                            Card_age, is_basic_land, Maindeck, DRAFT_CARDS_ID,
                                                                       Type_exact, Subtype,
                                                                            image_id_new)]
  testDeck[is.na(testDeck)] <- -1
  rest_sort <- create_deck_filters(testDeck, remove_extra = FALSE)


  rest <- rest_sort[!filter %in%  c("Name", "image_id", "DRAFT_CARDS_ID", "image_id_new")]

  fluidRow(
    lapply(1:nrow(rest), function(filtterLoop){
      rowData <- rest[filtterLoop]
      column(width = 1,

             checkboxGroupInput(inputId = rowData[, filter], label = rowData[, filter], choices = rowData[, options][[1]],
                                selected = rowData[, options][[1]]))
    }),
    column(width = 1,
           selectInput(inputId =  "row_sort", label = "Row sort", choices = rest_sort[, filter])),
    column(width = 1,
           selectInput(inputId = "col_sort", label = "Column sort", choices = rest_sort[, filter]))

  )
})


table_to_render_react <- reactive({
  #take dep
  input$update_deck_filter
  input$choose_decklist
  #DONT DEL ME UP


  testDeck <- ADM_VISUALIZE_CARDS[Pakka_ID == input$choose_decklist, .(Converted_Cost, Name, Rarity, Colors, Power, Toughness,
                                                                            Card_age, is_basic_land, Maindeck, DRAFT_CARDS_ID,
                                                                       Type_exact, Subtype,
                                                                            image_id_new)]

  testDeck[is.na(testDeck)] <- -1


  rest <- create_deck_filters(testDeck)
  #create filter data
  filter_dataset <- data.table(filter = as.character(), options = as.character())
isolate({
  for (filter_input_loop in 1:nrow(rest)) {
    row_data <- rest[filter_input_loop]
    new_row <- data.table(filter = row_data[, filter], options = list(c(input[[row_data[, filter]]],"ADDED_TO_PURKKA_WHEN_ONLY_1_SELECTED")))
    filter_dataset <- rbind(filter_dataset, new_row, fill = TRUE)
  }
  #ressit <- rest[1, .(subset_list = list(options[[1]][1:5])), by = filter]
print(filter_dataset)
  ressit <- filter_dataset
  total_string <- NULL

  for(koodiloop in 1:nrow(ressit)) {
    eka_rivi <-   paste0(ressit[koodiloop, filter], ' %in% ' ,ressit[koodiloop, options])
    if(koodiloop > 1) {
      total_string <- paste0(eka_rivi, " & ", total_string)
    } else {
      total_string <- paste0(eka_rivi)
    }
  }
})
  syntax <- (total_string)

  filtered_deck <- testDeck[eval(parse(text = syntax))]

  row_dim <- input$row_sort
  column_sort_dim <- input$col_sort

  sorted_deck <-  filtered_deck[order(get(row_dim), get(column_sort_dim))]
  #add x and y coordinates
  table_to_render <- sorted_deck[,  .(x = get(row_dim), Name,DRAFT_CARDS_ID,image_id_new,
                   y = seq_len(.N)),
              by =  list(get(row_dim))]

  table_to_render[, get := NULL]
  #table_to_render[, image_id_new :=   paste0(x, Name, y)]
#print(table_to_render)
  js$refreshDragulaR("dragula")
  table_to_render

})



output$deck_editor_select_deck <- renderUI({
  #DEPENDS
  input$add_card_to_deck
  input$SaveRemove
  ############################

  required_data("ADM_CURRENT_PAKKA")
  deck_sorted <- ADM_CURRENT_PAKKA[order(Pakka_Name)]
  deck_options <- deck_sorted[, Pakka_ID]
  names(deck_options) <- deck_sorted[, Pakka_Name]
  radioButtons(inputId = "choose_decklist", "Choose decklist", choices = deck_options, inline = TRUE)
})


#tab_deck_editor
output$decklist <- renderUI({
req( table_to_render_react())
  req(input$update_deck_filter)
  #take dep

print("PIIRRETAAN")

  #DONT DEL ME UP
  table_to_render <- table_to_render_react()
  required_data("STG_CARDS_DIM")
  sscols_cards <- STG_CARDS_DIM[, .(MID, Name)]
table_to_render[, drag_ID:= paste0("Drag", x)]
agg_to_x <- table_to_render[, .N, by = .(drag_ID, x)]


  fluidPage(

    lapply(table_to_render[, unique(x)], function(rivi) {

      column(id = paste0(agg_to_x[x == rivi, drag_ID]),
             width = 1,
            offset = 0,
             lapply(table_to_render[x == rivi, unique(y)], function(sarake) {
               kuva_id <- table_to_render[x == rivi & y == sarake, image_id_new]
               card_name <-  table_to_render[x == rivi & y == sarake, Name]
               MIDi <- sscols_cards[Name == card_name, MID]
            #    print(paste0(card_name, " ", sarake, " ", rivi))
               if (!exists(kuva_id, where = output)) {
                 getCardImg_full(MIDi)
               output[[kuva_id]] <-  renderImage({

                 list(src = paste0("./www/", MIDi, "_card.jpg"),#image_nm,
                      alt = "Image failed to render",
                      width = "150px"
                 )
               }, deleteFile = FALSE)
               }
               # tags$div(drag = kuva_id, imageOutput(kuva_id,
               #             height = "100px"))
               tags$div(drag = kuva_id, imageOutput(kuva_id,
                                                    height = "100px"))
               #tags$img(src = paste0(MIDi, "_card.jpg"), height = "200px", drag = kuva_id)
               # HTML(paste0('<div drag = "drag_',
               #             kuva_id,
               #             '" ',
               #             imageOutput(kuva_id,
               #                         height = "100px"),
               #             '</div>'))
             })
      )
    })



  )

})



output$dragOut <- renderDragula({
  #DEPEND
  table_to_render_react()
  input$update_deck_filter
  ##
 table_to_render <- table_to_render_react()[1 != 0]
 table_to_render[, drag_ID:= paste0("Drag", x)]
agg_to_x <- table_to_render[, .N, by = .(drag_ID, x)]
print("DRAGULA")
print(table_to_render)
#browser()
#
dragula(c("removeCard", as.character(agg_to_x[,drag_ID])))

#dragula(c("Drag0", "Drag1", "Drag2", "Drag3", "Drag4" ,"Drag5"))
 #dragula(c("Drag0","Drag1"))
})

