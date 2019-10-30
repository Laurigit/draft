draftMID <- reactiveValues(MID = NULL)

observeEvent(input$reload, {

  #leike <- gsub("[^0-9.-]", "", input$clip)


  leike <- readCardsFromTextArea(input$clip, con)
  draftiTaulu <- data.table(MID = leike)
  draftiTaulu[, PICK_ORDER := seq_len(.N)]
  draftiTaulu[, PICK_DT := as.IDate(Sys.Date(), tz = "EET")]
  draftiTaulu[, OMISTAJA_ID := omistaja_ID_calc$value]

  #find next draft no
  draft_data <- dbQ("SELECT max(DRAFT_ID) as max_draft, OMISTAJA_ID from DRAFT_CARDS GROUP BY OMISTAJA_ID",  con)
  if (nrow(draft_data[OMISTAJA_ID == omistaja_ID_calc$value]) == 0) {
    drafti_no <- 1
  } else {
    drafti_no <- as.numeric(draft_data[OMISTAJA_ID == omistaja_ID_calc$value, max_draft]) + 1
  }
  draftiTaulu[, DRAFT_ID := drafti_no]
  draftiTaulu[, PICKED := 0]

  draftMID$MID <- draftiTaulu

})




#näytä imaget
output$draftit <- renderUI({

  req(draftMID$MID)
  con <- connDB(con)
  input$reload

  leike <- draftMID$MID[, MID]
  #tee uniikit imageIdt
  uudet_kortit <- data.table(MID = leike)
  uudet_kortit[, image_id := paste0("Draft", seq_len(.N))]

  for (i in 1:nrow(uudet_kortit)) {

    local({
      my_i <- i
      image_id <- uudet_kortit[i, image_id]
      # print(image_id)
      image_nm <- paste0(uudet_kortit[i, MID], "_card.jpg")
      # print(image_nm)
      output[[image_id]] <-  renderImage({

        # output[[image_id]] <-  renderImage({
        list(src = paste0("./www/",image_nm),#image_nm,
             alt = "Image failed to render"
        )
      }, deleteFile = FALSE)
    })
  }




  fluidRow(
    lapply( 1:nrow(uudet_kortit), function(x) {
      column(width = 4,
             imageOutput(uudet_kortit[x, image_id],
                         height = "150px"
                         #  hover = hoverOpts(id = nimi)
             )
      )

    })
  )

})










observeEvent(input$draft_cards, {

kortit <-   draftMID$MID
  dbWriteTable(con, "DRAFT_CARDS", kortit, append = TRUE, row.names = FALSE)
  draftMID$MID <- NULL
  updateData("SRC_DRAFT_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())
})
