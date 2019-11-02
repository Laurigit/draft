#tab_new_deck
observeEvent(input$add_deck,{
  required_data("SRC_DECKS_DIM")
  str(SRC_DECKS_DIM)
  rivi_id <- SRC_DECKS_DIM[, max(rivi_id)] + 1
  free_pakka_no <- SRC_DECKS_DIM[Omistaja_nimi == session$user, max(Pakka)] + 1
  omistaja_kirain <-  str_sub(session$user, 1, 1)
new_row <- data.table(Nimi = input$pakka_nimi,
                      Divari = 1,
                      rivi_id = rivi_id,
                      Picked = 1,
                      Omistaja = omistaja_kirain,
                      Pakka = free_pakka_no,
                      Manastack_Deck_ID = "",
                      Json_Prefix = paste0(omistaja_kirain, "_", free_pakka_no),
                      Retired = 0,
                      Side = 0,
                      Manastack_name_url = ""

)
con <- connDB(con)
dbWriteTable(con, "DECKS_DIM", new_row, row.names = FALSE, append = TRUE)
required_data("ADM_DI_HIERARKIA")
updateData("SRC_DECKS", ADM_DI_HIERARKIA, globalenv())
})

observeEvent(input$load_new_deck_MIDS, {
  # con <- connDB(con)
  # kortit <- readCardsFromTextArea(input$new_deck_text_area, con)
  # required_data(c("STG_CARDS", "STG_DECKS_DIM"))
  # #session <- NULL
  # #session$user <- "Lauri"
  # mysidet <- STG_DECKS_DIM[Omistaja_ID == str_sub(session$user, 1, 1) & Side == 1, Pakka_ID]
  # my_latest_side_PFI <- STG_CARDS[Pakka_ID %in% mysidet, .(Pakka_form_ID = max(Pakka_form_ID)), by = Pakka_ID]
  # my_latest_side_decklists <- STG_CA
  # sidelaput <- STG_CARDS[Maindeck == 0]
  # #get kortit from sides
  #
  #
  # Free_draft_ID <- dbQ("SELECT MAX(DRAFT_CARDS_ID) FROM CARDS")


})

