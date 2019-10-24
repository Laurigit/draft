#tab_sidebar

output$select_draft <- renderUI({
  input$draft_cards

# omistaja_ID_calc <-  NULL
# omistaja_ID_calc$value <- "L"

  mun_draftatyt <- dbQ(paste0("SELECT count(MID) as count_MID, DRAFT_ID
                              FROM DRAFT_CARDS
                              WHERE PICKED = 0 AND OMISTAJA_ID = \"", omistaja_ID_calc$value,
                       "\" GROUP BY DRAFT_ID"),
                       con)

  selectInput(inputId = "todo_Drafts",
              label = "Select Draft_ID",
              choices = c(mun_draftatyt[, DRAFT_ID]))


})


#näytä imaget sidebarissa
ReactDraftCards <- reactiveValues(image_ids = NULL,
                                  cards_left = NULL)

observeEvent(input$todo_Drafts,{
req(input$todo_Drafts)
  uudet_kortit <- dbQ(paste0("SELECT MID, id as DRAFT_CARDS_ID
                              FROM DRAFT_CARDS
                             WHERE DRAFT_ID = ", input$todo_Drafts, " AND
                             PICKED = 0 AND
                             OMISTAJA_ID = \"", omistaja_ID_calc$value, "\""),
                      con)


  #tee uniikit imageIdt

  uudet_kortit[, image_id := paste0("DraftBar", seq_len(.N))]
  ReactDraftCards$image_ids <- uudet_kortit[, .(MID, image_id, DRAFT_CARDS_ID)]
  ReactDraftCards$cards_left <- uudet_kortit[, .(MID, image_id, DRAFT_CARDS_ID)]

}, ignoreNULL = TRUE, ignoreInit = TRUE)


output$draftitSideBar <- renderUI({
  req(input$todo_Drafts)
  print("kuvat pitäs päivittyy")
  input$todo_Drafts
  values$lastUpdated


  uudet_kortit <- ReactDraftCards$image_ids


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

#piirretään vaan jäljellä olevat
 jaljella_olevat <- ReactDraftCards$cards_left
print(jaljella_olevat)
  fluidRow(
    column(width = 11, offset = 1,
           lapply( 1:nrow(jaljella_olevat), function(x) {
             MIDi <- jaljella_olevat[x, image_id]
             imageOutput(MIDi,
                         height = "40px",
                         dblclick = dblclickOpts(id = MIDi)
                         #  hover = hoverOpts(id = nimi)
             )


           })
    )
  )

})

observe({
  # req(ReactDraftCards$cards_left)
  # cards_left <- nrow(ReactDraftCards$cards_left)
  # if (cards_left == 0) {shinyjs::enable("saveDraftedCards")} else {
  #   shinyjs::disable("saveDraftedCards")
  # }
})

observeEvent(input$saveDraftedCards,{
  required_data(c("STG_CARDS", "STG_CARDS_DIM"))
  #need old list
  #create new decklist
  #save it with new pfi
  #old_decklist <- STG_CARDS[Pakka_form_ID == 250]
 # Pakka_IDt <- deck_changes$draft[, .N, by = Pakka_ID][, Pakka_ID]
drafikortit <-  deck$changes[source ==  paste0("Draft", input$select_draft)]
  Pakka_IDt <- drafikortit[, .N, by = Pakka_ID][, Pakka_ID]
  for (pakkaloop in Pakka_IDt) {
    new_DCIDs <- drafikortit[Pakka_ID == pakkaloop, .( DRAFT_CARDS_ID)]
    new_dl_loop <- createNewDecklist_after_draft(new_DCIDs, pakkaloop, STG_CARDS, STG_CARDS_DIM, STG_DRAFT_CARDS)
    browser()
    #ammutaan kantaan
    dbWriteTable(con, "CARDS", new_dl_loop, row.names = FALSE, append = TRUE)
    required_data("ADM_DI_HIERARKIA")
    updateData("SRC_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())
  }

  #merkkaa dräfätyt
  draftit <- drafikortit[, .(id = DRAFT_CARDS_ID, PICKED = 1)]
  dbIoU("DRAFT_CARDS", draftit, con)
  updateData("SRC_DRAFT_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())
  deck$changes <-  deck$changes[1 == 0]

}, ignoreInit = TRUE, ignoreNULL = TRUE)
