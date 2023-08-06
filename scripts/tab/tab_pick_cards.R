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
    tags$img(src = rividata[, filu], height = "200px", drag = rividata[, kuva_id])

  })


})



observeEvent(input$save_picks, {
  browser()
  if (input$radio_first_pick_correct == "Lauri") {
    fp_in_data <- 0
  } else if (input$radio_first_pick_correct == "Martti") {
    fp_in_data <- 1
  } else {
    fp_in_data <- -1
  }

  picke_order <- data.table(kuva_id = unlist(input$dragula))
  picke_order[, PICK_ORDER := seq_len(.N)]
  picke_order[, MID := as.integer(word(kuva_id, 4, sep = fixed("_")))]
  picke_order[, Booster_ID := input$booster_selector]
  picke_order[, Omistaja_ID := str_sub(session$user, 1, 1)]
  picke_order[, first_pick := fp_in_data]
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
observeEvent(input$button_delete_booster, {
  updateSelectInput(
    session = getDefaultReactiveDomain(),
    inputId = input$allow_delete_booster,
    selected = "No"
  )
  shinyjs::disable("button_delete_booster")
  dbQ(paste0("DELETE FROM DRAFT_BOOSTER WHERE Booster_ID = ", input$booster_selector), con)
  updateData("SRC_DRAFT_BOOSTER", ADM_DI_HIERARKIA, globalenv())
  global_update_data$update <- isolate(global_update_data$update + 1)
  })

observeEvent(input$allow_delete_booster, {

  if (input$allow_delete_booster == "No") {
    shinyjs::disable("button_delete_booster")}
  else {shinyjs::enable("button_delete_booster")}
})

observeEvent(input$random_first_pick_correct, {
  rand_val <-  runif(1)
  print(rand_val)
  if (rand_val <  0.5 ) {
    updateSelectInput(inputId = "radio_first_pick_correct", session, selected = "Lauri")
  } else {
    updateSelectInput(inputId = "radio_first_pick_correct", session, selected = "Martti")
  }

})

observeEvent(input$lock_first_pick, {

  #0 on lauri, 1 on martti fp
  if (input$radio_first_pick_correct == "Lauri") {
    fp_in_data <- 0
  } else if (input$radio_first_pick_correct == "Martti") {
    fp_in_data <- 1
  } else {
    fp_in_data <- -1
  }

  dbQ(paste0("UPDATE DRAFT_BOOSTER SET first_pick = ", fp_in_data, " WHERE Booster_ID =", input$booster_selector), con)
  updateData("SRC_DRAFT_BOOSTER", ADM_DI_HIERARKIA, globalenv())
  global_update_data$update <- isolate(global_update_data$update + 1)

})

observeEvent(input$unlock_first_pick,{
  shinyjs::enable("lock_first_pick")
  shinyjs::enable("radio_first_pick_correct")
  shinyjs::enable("random_first_pick_correct")
})

observe({

  req(input$booster_selector)

  # I monitor if first pick has been selected for currently active booster
  global_update_data$update
  required_data("STG_DRAFT_BOOSTER")
  if (STG_DRAFT_BOOSTER[Booster_ID == input$booster_selector , .N, by = first_pick][, first_pick] == -1) {
    #not selected first pick
    shinyjs::enable("lock_first_pick")
    shinyjs::enable("radio_first_pick_correct")
    shinyjs::enable("random_first_pick_correct")
    updateSelectInput(inputId = "radio_first_pick_correct", session, selected = "Not selected")
  } else {
    shinyjs::disable("lock_first_pick")
    shinyjs::disable("radio_first_pick_correct")
    shinyjs::disable("random_first_pick_correct")
    if (STG_DRAFT_BOOSTER[Booster_ID == input$booster_selector , .N, by = first_pick][, first_pick] == 0) {
      updateSelectInput(inputId = "radio_first_pick_correct", session, selected = "Lauri")
    } else if (STG_DRAFT_BOOSTER[Booster_ID == input$booster_selector , .N, by = first_pick][, first_pick] == 1) {
      updateSelectInput(inputId = "radio_first_pick_correct", session, selected = "Martti")

    }
  }
})


