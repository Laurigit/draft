printChanges <- function(change_dt, input_ADM_VISUALIZE_CARDS, input_STG_CARDS_DIM, input_STG_DECKS_DIM) {


  # new_row <- isolate(data.table(source = paste0("Side", input$myDecks),
  #                               MID = changed_MID,
  #                               Pakka_ID = input$myDecks,
  #                               DRAFT_CARDS_ID = draft_card_id,
  #                               Maindeck = 1,
  #                               Removed_from_game = FALSE))



  text_tot <- NULL

  #input_ADM_VISUALIZE_CARDS <- ADM_VISUALIZE_CARDS
 # input_STG_CARDS_DIM <- STG_CARDS_DIM
  # input_STG_DECKS_DIM <- STG_DECKS_DIM

  for(loop_row in 1:nrow(change_dt)) {
    row_data <- change_dt[loop_row]
    card <- input_STG_CARDS_DIM[row_data[, MID] == MID, Name]
    toDeck <- word(input_STG_DECKS_DIM[Pakka_ID == row_data[, Pakka_ID], Nimi], 1, 1, sep = fixed("_"))
print(row_data)
sidetext <- ""
prefix <- ""
    if (row_data[, Maindeck] == 0) {
      sidetext <- "_Side"
      prefix = ""
    } else if (row_data[, Maindeck] == -1) {

      prefix <- " rem from "
    }
    text <- paste0(card, prefix, toDeck, sidetext, "<br>")
    text_tot <- paste0(text_tot, text)
  }
  return(text_tot)
}
