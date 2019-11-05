#tab_new_deck
observeEvent(input$add_deck,{
con <- connDB(con)
  #first create new deck
required_data("ADM_DI_HIERARKIA")
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
                        Manastack_name_url = "")
  dbIns("DECKS_DIM", new_row, con)



  new_deck_list <- rbind(rv_new_deck$main, rv_new_deck$side)#[1:4]
  #new_deck_list <- new_deck_list[1:4]
  #new_deck_list <- data.table(MID = c(425866, 397755,397796,442047,442207,442207), monesko_kortti = c(1, 1, 1, 1, 1, 2), Maindeck = c(1,1,1,1,0,0))
  required_data(c("STG_CARDS", "STG_DECKS_DIM"))
  #session <- NULL
  #session$user <- "Lauri"
  mysidet <- STG_DECKS_DIM[Omistaja_ID == str_sub(session$user, 1, 1) & Side == 1, Pakka_ID]
  my_latest_side_PFI <- STG_CARDS[Pakka_ID %in% mysidet, .(Pakka_form_ID = max(Pakka_form_ID)), by = Pakka_ID]
  my_latest_side_decklists <- STG_CARDS[Pakka_form_ID %in% my_latest_side_PFI[, Pakka_form_ID], .(MID, Pakka_ID, DRAFT_CARDS_ID, Name, monesko_kortti)]
  #my_latest_side_decklists[monesko_kortti== 2]
  #my_latest_side_decklists[MID == 397879]

  #get kortit from sides
  joini <- my_latest_side_decklists[new_deck_list, on = .(MID, monesko_kortti)]




  #create new sides
  changed_sides <- new_deck_with_info[, .N, by = Pakka_ID][, Pakka_ID]
  con <- connDB(con)
  for(update_sides in changed_sides) {
    remove <- new_deck_with_info[Pakka_ID == update_sides, DRAFT_CARDS_ID]
    new_side <- remove_DIDs_from_deck(update_sides, remove, STG_CARDS, STG_DECKS_DIM, con)
    dbWriteTable(con, "CARDS", new_side)
    updateData("SRC_CARDS", ADM_DI_HIERARKIA, globalenv())
  }



  #Free_PFI <- 500
  Free_PFI <- dbQ("SELECT MAX(Pakka_form_ID) FROM CARDS") + 1
  Pakka_ID <-  rivi_id
  new_deck_with_info <- cbind(joini, Pakka_form_ID = Free_PFI, Pakka_ID, CARD_ID = NULL)
  new_deck_with_info[, monesko_kortti := NULL]
  dbWriteTable(con, "CARDS", new_deck_with_info)
  updateData("SRC_CARDS", ADM_DI_HIERARKIA, globalenv())


#)
# con <- connDB(con)
# dbWriteTable(con, "DECKS_DIM", new_row, row.names = FALSE, append = TRUE)
# required_data("ADM_DI_HIERARKIA")
# updateData("SRC_DECKS", ADM_DI_HIERARKIA, globalenv())
})


rv_new_deck <- reactiveValues(main = NULL,
                              side = NULL)

observeEvent(input$load_new_deck_MIDS, {
  #con <- connDB(con)
  #input <- NULL
  #rv_new_deck <- NULL
  #input$new_dect_text_area <- "40001 40002 40003 40001 40001 40002 40005"
  kortit <- readCardsFromTextArea(input$new_deck_text_area, con)
  #kortit <- c(40001, 40002, 40002, 40003)
  kortti_dt <- data.table(MID = kortit)
  kortti_dt[, monesko_kortti := seq_len(.N), by = MID]
  if (input$load_to_main_or_side == "Main") {

        rv_new_deck$main <- kortti_dt
        rv_new_deck$side[, Maindeck := 1]

  } else {
        rv_new_deck$side <- kortti_dt
        rv_new_deck$side[, Maindeck := 0]

  }


})



