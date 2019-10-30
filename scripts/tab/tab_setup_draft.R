#tab_setup_draft

setupDraft <- reactiveValues(cards = NULL)

observeEvent(input$load_setup_draft, {
print("Painettu")
setupDraft$cards <- readCardsFromTextArea(input$loadSetupDraft_area)
print(setupDraft$cards)

})

output$dnd_draft <- renderUI({
  #req(setupDraft$cards)
  kuvadata <- data.table(MID = setupDraft$cards)
  print(input$load_setup_draft)

kuvadata[, rivi := seq_len(.N)]
  kuvadata[, kuva_id := paste0("id_", rivi)]
  kuvadata[, filu := paste0(MID, "_card.jpg")]


  lapply(1:nrow(kuvadata), function(x) {

      rividata <- kuvadata[x]
print(rividata)
     tags$img(src = rividata[, filu], height = "200px", drag = rividata[, kuva_id])
     # dragUI(id = paste0("kuva_Id_setup", x), tags$img(src = "464104_card.jpg"), height = "200px"))

  })

})



# output$dragula <- renderDragula({
#   dragula(c("draft", "picks"))
# })


output$dragula <- renderDragula({
  dragula(c("Available", "Model"))
})


output$order <- renderPrint({
  req(setupDraft$cards)
  dragulaValue(input$dragula)
})


output$dnd_picks <- renderUI({
  # pick_order <- 1:15
  # fluidRow(
  #     lapply(pick_order, function(x) {
  #       column(1, tags$h3(id = paste0("pick_", x)))
  #
  #     }
  #   )
  # )
  div()
})
  # files <- ADM_CARD_IMAGES[1:10, file_name]
  # lapply(files, function(x) {
  #   fluidRow(
  #     dragUI(id = paste0("kuva_Id", x), tags$img(src = x, height = "200px"))
  #   )
  # })







