#tab_delete_deck



observeEvent(input$confirm_retire, {
  required_data(c("STG_DECKS_DIM", "ADM_DI_HIERARKIA"))
  # input <- NULL
  # input$delete_deck <- "L8_UG_Insect"

  update_row <- (SRC_DECKS_DIM[Nimi == input$delete_deck])

  dbQ("SET SQL_SAFE_UPDATES = 0", con)
  dbQ(paste0('UPDATE DECKS_DIM SET Divari = NULL, Picked = NULL, Retired = 1 WHERE Nimi = "', input$delete_deck, '"'), con)
  #dbIoU("DECKS_DIM", update_row, con)
  updateData("SRC_DECKS_DIM", ADM_DI_HIERARKIA, globalenv() )

})
