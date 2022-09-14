#tab_resolve_draft

output$show_resolvable_drafts <- renderUI({

  #DEPENDENCY DONT DEL
  print(input$save_picks)
  print(global_update_data$update)
  ###########

  required_data("ADM_DI_HIERARKIA")
  required_data("STG_DRAFT_PICKORDER")
  required_data("SRC_DRAFT_PICKORDER")

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

resolved_draft <- reactiveValues(cards = NULL)
output$pickorders <- renderUI({
  # input <- NULL
  # input$select_draft_to_resolve <- 1
  #input$radio_first_pick <- "M"
  # mda <- cardData
  # mda[, OMISTAJA_ID := "M"]
  # mda[, PICK_ORDER := sample(1:16)]
  # cardData <- rbind(cardData, mda)
  # cardData[, MID := ifelse(MID == 10, 3, MID)]
  required_data("STG_DRAFT_PICKORDER")
  cardData <- STG_DRAFT_PICKORDER[Booster_ID == input$select_draft_to_resolve]
  cardData[, PICK_ORDER := ifelse(OMISTAJA_ID == str_sub(input$radio_first_pick, 1, 1), PICK_ORDER - 0.5, PICK_ORDER)]
  cardData_sorted <- cardData[order(PICK_ORDER)]
  cardsLeft <- cardData_sorted[, .(card_count = .N / 2) , by = MID]
  all_picks <- NULL
  picki_vuorot <- cardData_sorted[, .(OMISTAJA_ID)]

  for (picki_for in 1:(nrow(cardData_sorted) / 2)) {
      #kuka pickaa?
    pick_vuoro_omistaja <- picki_vuorot[picki_for, OMISTAJA_ID]

    repeat{

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
  all_picks[, Booster_ID := input$select_draft_to_resolve]
  resolved_draft$cards <- all_picks
  all_picks[, rivi := seq_len(.N)]

  omistajat <- c("L", "M")

  lapply(omistajat, function(y) {
column(3,

           lapply(all_picks[OMISTAJA_ID == y, rivi], function(x) {
          #   tags$div(style= paste0('width: 100px; height: 100px; background-image: ', all_picks[x, MID], '_card.jpg; background-repeat: no-repeat;'))
           # imageOutput("")
           #  HTML('div(class="topimg",img(src=464104_card.jpg,height="100%", width="100%"))))')

#   style = "object-fit: cover;"),
              tags$img(src = paste0(all_picks[x, MID], "_card.jpg"),  height = "220px")
                    # style = 'object-fit = "cover";height = 100px;')
           })

    )
           })





})

observeEvent(input$accept_and_save,{
  required_data("SRC_DRAFT_PICKORDER")
  to_save <- resolved_draft$cards
  to_save[, rivi := NULL]
  delete_booster_id <- to_save[, max(Booster_ID)]
  to_save[, Booster_ID := NULL]
  con <- connDB(con)

  maxDraft <- as.numeric(dbQ("SELECT MAX(DRAFT_ID) FROM DRAFT_CARDS", con)) + 1
  if (is.na(maxDraft)) {
    maxDraft <- 1
  }

  add_info <- cbind(to_save , PICK_DT = as.IDate(Sys.Date(), tz = "EET"), PICKED = 0, DRAFT_ID = maxDraft)
  dbWriteTable(con, "DRAFT_CARDS", add_info, append = TRUE, row.names = FALSE)
  updateData("SRC_DRAFT_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())

  del_Query <- paste0("DELETE FROM DRAFT_PICKORDER WHERE Booster_ID = ", input$select_draft_to_resolve)
  dbQ(del_Query, con)
  updateData("SRC_DRAFT_PICKORDER", ADM_DI_HIERARKIA, input_env = globalenv())


  #delete cards from DRAFT_BOOSTER
  #delete_booster_id <- 1

  dbQ(paste0("DELETE FROM DRAFT_BOOSTER WHERE Booster_ID = ", delete_booster_id), con)
  updateData("SRC_DRAFT_BOOSTER", ADM_DI_HIERARKIA, globalenv())
  global_update_data$update <- isolate(global_update_data$update + 1)
})
