#tab_resolve_draft

output$show_resolvable_drafts <- renderUI({

  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_DRAFT_PICKORDER", ADM_DI_HIERARKIA, globalenv())
  if (nrow(STG_DRAFT_PICKORDER) > 0) {
    aggr <- STG_DRAFT_PICKORDER[, .N, by = .(OMISTAJA_ID, Booster_ID)]
    result <- aggr[, .N, by = Booster_ID][N == 2, Booster_ID]
  } else {
    result <- "No drafts to resolve"
  }

  selectInput(inputId = "select_draft_to_resolve", label = "Select booster to resolve", choices = result)
})

observeEvent(input$random_first_pick, {
  if (runif(1) <  0.5 ) {
    updateSelectInput(inputId = "radio_first_pick", session, selected = "Lauri")
  } else {
    updateSelectInput(inputId = "radio_first_pick", session, selected = "Martti")
  }

})


output$pickorders <- renderUI({
  # input <- NULL
  # input$select_draft_to_resolve <- 1
  #input$radio_first_pick <- "M"
  # mda <- cardData
  # mda[, OMISTAJA_ID := "M"]
  # mda[, PICK_ORDER := sample(1:16)]
  # cardData <- rbind(cardData, mda)
  # cardData[, MID := ifelse(MID == 10, 3, MID)]
  cardData <- STG_DRAFT_PICKORDER[Booster_ID == input$select_draft_to_resolve]
 # cardData[, PICK_ORDER := ifelse(OMISTAJA_ID == str_sub(input$radio_first_pick, 1, 1), PICK_ORDER - 0.5, PICK_ORDER)]
  cardData_sorted <- cardData[order(PICK_ORDER)]
  cardsLeft <- cardData_sorted[, .(card_count = .N / 2) , by = MID]
  all_picks <- NULL
  picki_vuorot <- cardData_sorted[, .(OMISTAJA_ID)]

  for (picki_for in 1:(nrow(cardData_sorted) / 2)) {
      #kuka pickaa?
    pick_vuoro_omistaja <- picki_vuorot[picki_for, OMISTAJA_ID]
    print(picki_for)
    repeat{
      print("repeat")
      #mitä pickaa
      minPick <- cardData_sorted[OMISTAJA_ID == pick_vuoro_omistaja, min(PICK_ORDER)]
      try_pick_MID <- cardData_sorted[OMISTAJA_ID == pick_vuoro_omistaja & PICK_ORDER == minPick, MID]
      #jos kortit löytyy tai kortit on pussista loppu, niin keskeytä
      if (nrow(cardsLeft[MID %in%  try_pick_MID]) > 0 | nrow(cardsLeft) == 0)  {
        break()
      }
        #ei löytynyt, siivoo dataa
      cardData_sorted <- cardData_sorted[!(OMISTAJA_ID == pick_vuoro_omistaja & PICK_ORDER == minPick)]
    }


    pickedMID <- data.table(MID = try_pick_MID, OMISTAJA_ID = pick_vuoro_omistaja, PICK_ORDER = picki_for)
    cardsLeft[MID == pickedMID[, MID], card_count := card_count - 1]
    cardsLeft <- cardsLeft[card_count > 0]
    all_picks <- rbind(all_picks, pickedMID)
  }
  all_picks[, rivi := seq_len(.N)]
  omistajat <- all_picks[, .N, by = OMISTAJA_ID][, OMISTAJA_ID]
  fluidRow(
  #lapply(OMISTAJA_ID, function(x)){
   # column(1,
           lapply(all_picks[OMISTAJA_ID == "L", rivi], function(x) {
              tags$img(src = paste0(all_picks[x, MID], "_card.jpg"))
           })

      #     )

  #         )


  )


  # con <- connDB(con)
  # maxDraft <- as.numeric(dbQ("SELECT MAX(DRAFT_ID) FROM DRAFT_CARDS", con)) + 1
  # add_info <- cbind(all_picks, PICK_DT = as.IDate(Sys.Date(), tz = "EET"), PICKED = 0, DRAFT_ID = maxDraft)
  # dbWriteTable(con, "DRAFT_CARDS", add_info, append = TRUE, row.names = FALSE)
  # updateData("SRC_DRAFT_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())


})
