#tab_setup_draft

setupDraft <- reactiveValues(cards = NULL,
                             result = NULL)

observeEvent(input$load_setup_draft, {
  print("Painettu")
  req(input$loadSetupDraft_area)
con <- connDB(con)
required_data("STG_CARDS_DIM")
  setupDraft$cards <- readCardsFromTextArea(input$loadSetupDraft_area, con, STG_CARDS_DIM)
  setupDraft$cards[, rivi := seq_len(.N)]
  setupDraft$cards[, kuva_id := paste0("id_", rivi)]
  setupDraft$cards[, filu := paste0(MID, "_card.jpg")]
  lapply(setupDraft$cards[, Name], function(x) {
    addCardToDB(x, con, STG_CARDS_DIM)
  })

  lapply(setupDraft$cards[, MID], function(x) {
    getCardImg_full(x)
  })

  print(setupDraft$cards)


})

observeEvent(input$save_to_be_drafted, {



setupDraft$result <- setupDraft$cards[, .(MID)]
BOOSTER_ID <- dbQ("SELECT MAX(BOOSTER_ID) as BOOSTER_ID FROM DRAFT_BOOSTER", con) + 1
if (is.na(BOOSTER_ID[, BOOSTER_ID])) {
  BOOSTER_ID <- 1
}
setupDraft$result <- cbind(setupDraft$result, BOOSTER_ID)
dbWriteTable(con, "DRAFT_BOOSTER", setupDraft$result, append = TRUE, row.names = FALSE)
updateData("SRC_DRAFT_BOOSTER", ADM_DI_HIERARKIA, input_env = globalenv(), FALSE)
setupDraft$cards <-  NULL
global_update_data$update <- isolate(global_update_data$update + 1)
}, ignoreNULL = TRUE, ignoreInit = TRUE)





output$dnd_draft <- renderUI({
print(setupDraft$cards)
  req(setupDraft$cards)
  kuvadata <- setupDraft$cards


        #dont del me
        print(input$load_setup_draft)
      #the up one



  lapply(1:nrow(kuvadata), function(x) {

      rividata <- kuvadata[x]
     tags$img(src = rividata[, filu], height = "200px", drag = rividata[, kuva_id])

  })

})





output$order <- renderPrint({
  req(setupDraft$cards)
  dragulaValue(input$dragula)
})



