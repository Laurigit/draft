#tab_setup_draft

setupDraft <- reactiveValues(cards = NULL,
                             result = NULL)

observeEvent(input$load_setup_draft, {
  print("Painettu")
  req(input$loadSetupDraft_area)

  setupDraft$cards <- data.table(MID = readCardsFromTextArea(input$loadSetupDraft_area, con))
  setupDraft$cards[, rivi := seq_len(.N)]
  setupDraft$cards[, kuva_id := paste0("id_", rivi)]
  setupDraft$cards[, filu := paste0(MID, "_card.jpg")]


  print(setupDraft$cards)

})

observeEvent(input$save_to_be_drafted, {

 #  cards <-  setupDraft$cards
# picke_order <- data.table(kuva_id = unlist(cards$dnd_draft))
# picke_order[, PICK_ORDER := seq_len(.N)]
# join_mids <- picke_order[  setupDraft$cards, on = "kuva_id"]
# join_mids[, Omistaja_ID := session$user]
# join_mids[, kuva_id := NULL]
# join_mids[, rivi := NULL]
# join_mids[, filu := NULL]

setupDraft$result <- setupDraft$cards[, .(MID)]
BOOSTER_ID <- dbQ("SELECT MAX(BOOSTER_ID) as BOOSTER_ID FROM DRAFT_BOOSTER", con) + 1
if (is.na(BOOSTER_ID[, BOOSTER_ID])) {
  BOOSTER_ID <- 1
}
setupDraft$result <- cbind(setupDraft$result, BOOSTER_ID)
dbWriteTable(con, "DRAFT_BOOSTER", setupDraft$result, append = TRUE, row.names = FALSE)
updateData("SRC_DRAFT_BOOSTER", ADM_DI_HIERARKIA, input_env = globalenv(), FALSE)
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



# output$dragula <- renderDragula({
#   dragula(c("draft", "picks"))
# })




output$order <- renderPrint({
  req(setupDraft$cards)
  dragulaValue(input$dragula)
})


# output$dnd_picks <- renderUI({
#   # pick_order <- 1:15
#   # fluidRow(
#   #     lapply(pick_order, function(x) {
#   #       column(1, tags$h3(id = paste0("pick_", x)))
#   #
#   #     }
#   #   )
#   # )
#   div()
# })




