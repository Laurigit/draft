output$select_booster <- renderUI({

  ##############DEPP
  print(input$load_setup_draft)
  #dont del next print
  print(global_update_data$update)
  print(local_update_data$update)
  #############dont del

  required_data(c("ADM_UNPICKED_DRAFT"))
my_to_do <- ADM_UNPICKED_DRAFT[OMISTAJA_ID == omistaja_ID_calc$value, .N, by = Booster_ID][, .(Booster_ID)]

  if (nrow(my_to_do) == 0) {
    boosterlist <- "No boosters to draft"

  } else {
    boosterlist <- my_to_do[, Booster_ID]
  }
  selectInput(inputId = "booster_selector",
              label = "Select booster No",
              choices = boosterlist)

})

output$myLegalColors <- renderUI({
  required_data("STG_DECKS_DIM")
  colors <- STAT_CURRENT_PAKKA()
 # browser()
  My_active_decks <- STG_DECKS_DIM[Retired == 0 & Side == 0 & Omistaja_ID == omistaja_ID_calc$value, Pakka_ID]
  myDecks <- colors[Pakka_ID %in% My_active_decks, .N , by = Top2_colors][, Top2_colors]
  HTML(myDecks)
})

output$picK_order <- renderUI({
  kortit <- STG_DRAFT_BOOSTER[Booster_ID == input$booster_selector]
  kortit[, rivi := seq_len(.N)]
  kortit[, kuva_id := paste0("id_pick_", rivi, "_", MID)]
  kortit[, filu := paste0(MID, "_card.jpg")]


  lapply(1:nrow(kortit), function(x) {

    rividata <- kortit[x]
    tags$img(class = "draggable", src = rividata[, filu], height = "200px", drag = rividata[, kuva_id])

  })


})

useShinyjs(script = "
  $('.draggable').on('dragstart', function(event) {
    event.preventDefault();
  });
")


observeEvent(input$save_picks, {


  picke_order <- data.table(kuva_id = unlist(input$dragula))
  picke_order[, PICK_ORDER := seq_len(.N)]
  picke_order[, MID := as.integer(word(kuva_id, 4, sep = fixed("_")))]
  picke_order[, Booster_ID := input$booster_selector]
  picke_order[, Omistaja_ID := str_sub(session$user, 1, 1)]
  picke_order[, kuva_id := NULL]
  dbWriteTable(con, "DRAFT_PICKORDER", picke_order, row.names = FALSE, append = TRUE)
  updateData("SRC_DRAFT_PICKORDER", ADM_DI_HIERARKIA, globalenv())
  local_update_data$update <- isolate(local_update_data$update + 1)
})


# observeEvent(input$select_booster, {
#
#
#
# }, ignoreNULL = TRUE, ignoreInit = TRUE)


output$card_order_text <- renderUI({

  dtpicked <- data.table(unlist(input$dragula))[, MID := as.numeric(word(V1, 4, sep = fixed("_")))]
  dtpicked[, order := seq_len(.N)]

  required_data("STG_CARDS_DIM")
  sscol <- STG_CARDS_DIM[, .(Name, MID)]
  joinNimi <- sscol[dtpicked, on = "MID"]

  rivivaihto <- HTML(paste0(joinNimi[, Name], collapse = "<br>"))
  #browser()
  rivivaihto
})


observeEvent(input$dragula, {
  if (is.null(input$dragula)) {
    shinyjs::disable("save_picks")
  } else {
    shinyjs::enable("save_picks")
  }


})
