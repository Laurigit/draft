#tab_setup_draft

setupDraft <- reactiveValues(cards = NULL,
                             result = NULL,
                             cardCount = NULL)

observeEvent(input$load_setup_draft, {
  print("Painettu")
  req(input$loadSetupDraft_area)
con <- connDB(con)
required_data("STG_CARDS_DIM")

  setupDraft$cards <- readCardsFromTextArea(input$loadSetupDraft_area, con, STG_CARDS_DIM)
  setupDraft$cards[, rivi := seq_len(.N)]
  setupDraft$cards[, kuva_id := paste0("id_", rivi)]
  setupDraft$cards[, filu := paste0(MID, "_card.jpg")]
  setupDraft$cards[, loop_countteri := cumsum(Name == "Goblin Token")]
  lapply(setupDraft$cards[Name != "Goblin Token", Name], function(x) {
    print(x)
    addCardToDB(x, con, STG_CARDS_DIM)
  })

  lapply(setupDraft$cards[Name != "Goblin Token", MID], function(x) {
    getCardImg_full(x)
  })
setupDraft$cardCount <- setupDraft$cards[Name != "Goblin Token", .(countti = .N), by = loop_countteri][, countti]

print(setupDraft$cards)


})

output$card_count_output <- renderText({
  req(setupDraft$cardCount)
  setupDraft$cardCount
})

observeEvent(input$save_to_be_drafted, {



setupDraft$result <- setupDraft$cards[, .(MID, Name, loop_countteri)]

total_loops <- setupDraft$result[, max(loop_countteri)]


BOOSTER_ID <- dbQ("SELECT MAX(BOOSTER_ID) as BOOSTER_ID FROM DRAFT_BOOSTER", con) + 1
if (is.na(BOOSTER_ID[, BOOSTER_ID])) {
  BOOSTER_ID <- 1
}


for (booster_loop in 0:total_loops){
  loop_data <- setupDraft$result[loop_countteri == booster_loop & !Name %in% ("Goblin Token"), .(MID)]


loop_booster_id <- BOOSTER_ID + booster_loop
loop_data <- cbind(loop_data, loop_booster_id)
loop_data[, random_var := runif(1)]
dbWriteTable(con, "DRAFT_BOOSTER", loop_data, append = TRUE, row.names = FALSE)
updateData("SRC_DRAFT_BOOSTER", ADM_DI_HIERARKIA, input_env = globalenv(), FALSE)
}
setupDraft$cards <-  NULL
updateTextAreaInput(session, "loadSetupDraft_area", value = "")

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



