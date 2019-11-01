output$select_booster <- renderUI({

  required_data("STG_DRAFT_BOOSTER")

  if(nrow(STG_DRAFT_BOOSTER) == 0) {
    boosterlist <- "No boosters to draft"

  } else {
    boosterlist <- STG_DRAFT_BOOSTER[, .N, by = Booster_ID][, Booster_ID]
  }
  selectInput(inputId = "booster_selector",
              label = "Select booster No",
              choices = boosterlist)

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


  picke_order <- data.table(kuva_id = unlist(input$dragula))
  picke_order[, PICK_ORDER := seq_len(.N)]
  picke_order[, MID := as.integer(word(kuva_id, 4, sep = fixed("_")))]
  picke_order[, Booster_ID := input$booster_selector]
  picke_order[, Omistaja_ID := str_sub(session$user, 1, 1)]
  picke_order[, kuva_id := NULL]
  dbWriteTable(con, "DRAFT_PICKORDER", picke_order, row.names = FALSE, append = TRUE)
  updateData("SRC_DRAFT_PICKORDER", ADM_DI_HIERARKIA, globalenv())

})


observeEvent(input$select_booster, {



}, ignoreNULL = TRUE, ignoreInit = TRUE)
