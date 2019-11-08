#tab_delete_deck



observeEvent(input$confirm_retire, {
  required_data("STG_DECKS_DIM")
  input <- NULL
  input$delete_deck <- "LSLS"
  update_row <- STG_DECKS_DIM[Nimi == input$delete_deck]
  update_row[, ':=' (Divari = NA, Picked = NA, Retired = 1)]
  dbIoU("DECKS_DIM", update_row, con)

})
