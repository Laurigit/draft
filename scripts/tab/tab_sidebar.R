#tab_sidebar

output$select_draft <- renderUI({

  #dont del next print
  print(global_update_data$update)
  print(local_update_data$update)
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
  req( omistaja_ID_calc$value)

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



output$sideSelection <- renderUI({

 if(input$sideMenu == "Deck editor add") {
    uiOutput("add_cards_slot")
    } else {
    uiOutput("draftitSideBar")
  }
})


output$add_cards_slot <- renderUI({
  fluidPage(
    fluidRow(
      actionButton("AddCardToDeck", "Add card to deck")
    ),

    fluidRow(
      textInput("add_card_by_text", "Type card name"),
      actionButton("find_card", "Find card")
    ),
    fluidRow(
      actionButton("add_card_to_deck", "Add card to deck"),
        uiOutput("preview_added_card")

    )
  )
})

observeEvent(input$add_card_to_deck, {
  required_data(c("STG_CARDS_DIM", "STG_CARDS", "STG_DECKS_DIM"))

  con <- connDB(con)
  add_to_pakka_id <- input$choose_decklist

  new_dl_after_adding <- add_outlier_card_return_new_dl(input_card_name = input$add_card_by_text,
                                                      Pakka_ID_input = add_to_pakka_id, STG_CARDS, STG_CARDS_DIM, STG_DECKS_DIM, con)
  new_dl_after_adding[, Valid_from_DT := now(tz = "EET")]
  dbWriteTable(con, "CARDS", new_dl_after_adding, row.names = FALSE, append = TRUE)
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())

})

output$preview_added_card <- renderUI({
  #add dep
  input$find_card
  ###
  required_data("STG_CARDS_DIM")
  isolate(etsi_MID <- STG_CARDS_DIM[Name == input$add_card_by_text, MID])
  output$preview_add_card <-  renderImage({

    list(src = paste0("./www/", etsi_MID, "_card.jpg"),#image_nm,
         alt = "Image failed to render",
         width = "150px"
    )
  }, deleteFile = FALSE)
  tags$div(imageOutput("preview_add_card",
                                       height = "100px"))

})

#save card to db




# observeEvent(input$SaveRemove, {
#
#   remove_cards_list <- unlist(input$dragOut[["removeCard"]])
#
#   find_image_id <- table_to_render_react()[image_id_new %in% remove_cards_list, DRAFT_CARDS_ID]
#
#   required_data("STG_CARDS")
#   #con <- connDB(con)
#
#
#   remove_pakka_id <- input$choose_decklist
#   new_decklist <- remove_DIDs_from_deck(input_Pakka_ID = remove_pakka_id,
#                                         removed_DIDs = find_image_id, STG_CARDS, con)
#   new_decklist[, Valid_from_DT := now(tz = "EET")]
#
#   dbWriteTable(con, "CARDS", new_decklist, row.names = FALSE, append = TRUE)
#   required_data("ADM_DI_HIERARKIA")
#   updateData("SRC_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())
# })

output$draftitSideBar <- renderUI({
  req(input$todo_Drafts)
  print("kuvat pitäs päivittyy")
  input$todo_Drafts
  values$lastUpdated


  uudet_kortit <- ReactDraftCards$image_ids


  for (i in 1:nrow(uudet_kortit)) {
#print("sidekortit piirtyy")
    local({
      #print(i)
      my_i <- i
      image_id <- uudet_kortit[i, image_id]
      # print(image_id)
      image_nm <- paste0(uudet_kortit[i, MID], "_card_small.jpg")
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
             tags$div(imageOutput(MIDi,
                         height = "40px",
                         width = "100%",
                         dblclick = dblclickOpts(id = MIDi)
                         #  hover = hoverOpts(id = nimi)
                              ),
                      Width = "100%")


           })
    )
  )

})




observeEvent(input$saveDraftedCards,{
  required_data(c("STG_CARDS", "STG_CARDS_DIM", "STG_DECKS_DIM", "STG_DRAFT_CARDS"))
  #need old list
  #create new decklist
  #save it with new pfi
  #old_decklist <- STG_CARDS[Pakka_form_ID == 250]
 # Pakka_IDt <- deck_changes$draft[, .N, by = Pakka_ID][, Pakka_ID]
drafikortit <-  deck$changes[source ==  paste0("Draft", input$select_draft)]
  Pakka_IDt <- drafikortit[, .N, by = Pakka_ID][, Pakka_ID]
  for (pakkaloop in Pakka_IDt) {
    new_DCIDs <- drafikortit[Pakka_ID == pakkaloop, .( DRAFT_CARDS_ID)]
    new_dl_loop <- createNewDecklist_after_draft(new_DCIDs, pakkaloop, STG_CARDS, STG_CARDS_DIM, STG_DRAFT_CARDS, STG_DECKS_DIM)

    #ammutaan kantaan
    new_dl_loop[, Valid_from_DT := now(tz = "EET")]
    dbWriteTable(con, "CARDS", new_dl_loop, row.names = FALSE, append = TRUE)
    required_data("ADM_DI_HIERARKIA")
    updateData("SRC_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())
  }

  #merkkaa dräfätyt
  draftit <- drafikortit[, .(id = DRAFT_CARDS_ID, PICKED = 1)]
  dbIoU("DRAFT_CARDS", draftit, con)
  updateData("SRC_DRAFT_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())
  deck$changes <-  deck$changes[1 == 0]
  local_update_data$update <- isolate(local_update_data$update + 1)
}, ignoreInit = TRUE, ignoreNULL = TRUE)
