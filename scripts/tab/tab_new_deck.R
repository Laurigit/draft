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
dbWriteTable(con, "DECKS_DIM", new_row, row.names = FALSE, append = TRUE)
})
