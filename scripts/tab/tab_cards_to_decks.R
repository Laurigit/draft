#tab_cards_to_decks


observeEvent(input$toggle_saving, {
  shinyjs::toggle("save_drafts_to_decks")
})


observeEvent(input$save_drafts_to_decks, {
  required_data(c("STG_CARDS", "STG_CARDS_DIM", "STG_DECKS_DIM", "STG_DRAFT_CARDS"))
  #need old list
  #create new decklist
  #save it with new pfi
  #old_decklist <- STG_CARDS[Pakka_form_ID == 250]
  # Pakka_IDt <- deck_changes$draft[, .N, by = Pakka_ID][, Pakka_ID]
dragula_status_from_draft <- dragulaValue(input$drag_cards_to_deck)
how_many_slots_in_list <- length(dragula_status_from_draft)
full_data <- NULL
for(loopperi in 1:how_many_slots_in_list) {
  name <- names(dragula_status_from_draft)[[loopperi]]
  data <- dragula_status_from_draft[[loopperi]]
  if (!is.null(data)) {
  loop_dt <- data.table(card_id_data = data, Deck = name)
  full_data <- rbind(loop_dt, full_data)
  }
}
full_data[, image_id := word(card_id_data, 1, 1, sep = "_")]


draft_info_data <- ReactDraftCards_d2d$image_ids[full_data, on = "image_id"]
draft_info_data[, Nimi := ifelse(Deck == "Drafted_cards_column", ifelse(omistaja_ID_calc$value == "L", "L_Side",
                                                                        "M_Side"), Deck)]
ss_decks <- STG_DECKS_DIM[, .(Pakka_ID, Nimi)]
join_pakkaid <- ss_decks[draft_info_data, on = "Nimi"]


deck_changes_data <- join_pakkaid[, .(DRAFT_CARDS_ID, Pakka_ID)]




  #drafikortit <-  deck$changes[source ==  paste0("Draft", input$select_draft)]
  Pakka_IDt <- deck_changes_data[, .N, by = Pakka_ID][!is.na(Pakka_ID), Pakka_ID]
  pfi_looper <- STG_CARDS[, max(Pakka_form_ID)] + 1
  all_new_decklists <- NULL
  print(pfi_looper)

  for (pakkaloop in Pakka_IDt) {
    new_DCIDs <- deck_changes_data[Pakka_ID == pakkaloop, .( DRAFT_CARDS_ID)]
    new_dl_loop <- createNewDecklist_after_draft(new_DCIDs, pakkaloop, STG_CARDS, STG_CARDS_DIM, STG_DRAFT_CARDS, STG_DECKS_DIM,
                                                 input_pfi = pfi_looper)
    pfi_looper <- pfi_looper + 1

    #ammutaan kantaan
    new_dl_loop[, Valid_from_DT := now(tz = "EET")]
    all_new_decklists <- rbind(all_new_decklists, new_dl_loop)
   # print(new_dl_loop)
  #
  }
  dbWriteTable(con, "CARDS", all_new_decklists, row.names = FALSE, append = TRUE)
  required_data("ADM_DI_HIERARKIA")
   updateData("SRC_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())

  #merkkaa dräfätyt
  draftit <- deck_changes_data[, .(id = DRAFT_CARDS_ID, PICKED = 1)]
  dbIoU("DRAFT_CARDS", draftit, con)
  updateData("SRC_DRAFT_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())
  deck$changes <-  deck$changes[1 == 0]
  local_update_data$update <- isolate(local_update_data$update + 1)
})

output$deck_column <- renderUI({
  required_data("STG_DECKS_DIM")
  required_data("STAT_CURRENT_PAKKA")


  mydecks_dt <- STG_DECKS_DIM[Omistaja_ID == omistaja_ID_calc$value  & Picked == 1, .(Nimi)]
  ssdynamic <- STAT_CURRENT_PAKKA()[, .(Pakka_NM_Dynamic, Pakka_NM, Nimi = Pakka_NM, Cards_in_side, Cards_in_Main)]
  mydecks_dt_join <- ssdynamic[mydecks_dt, on = "Nimi"]
  mydecks <- mydecks_dt_join[, Pakka_NM]
  #mydecks_text <- mydecks_dt_join[, paste0(Pakka_NM_Dynamic, "<br>", Cards_in_Main, "/", Cards_in_side)]
  half_decks <- round(length(mydecks) / 2)


fluidPage(
              fluidRow(

                column(width = 3, code("First picks"), uiOutput("first_picks",
                                                                style = "min-height:100px; overscroll-behavior-y: contain; touch-action: none;")),
                              lapply(mydecks[1:half_decks], function(deck_name) {
                                deck_header <- mydecks_dt_join[Pakka_NM == deck_name, paste0(Pakka_NM_Dynamic, "<br>", Cards_in_Main, "/", Cards_in_side)]
                                column(width = 2, tags$h4(style = "color:red", HTML(deck_header)), uiOutput(deck_name, style = "min-height:100px;background-color:grey; overscroll-behavior-y: contain; touch-action: none;"))
                              })),
              fluidRow(column(width = 12,  style='padding:80px;',"")),
            fluidRow(
              column(width = 3, code("To sideboard"),
                     box(uiOutput("Drafted_cards_column", style = "max-width: 150px; height:380px; overflow-x: scroll: overscroll-behavior-y: contain; overscroll-behavior-y: contain; touch-action: none;"),
                         height = 380,
                         style = "overflow-x: scroll; min-width: 300px")
                     ),
              lapply(mydecks[(half_decks + 1):length(mydecks)], function(deck_name) {
                deck_header <- mydecks_dt_join[Pakka_NM == deck_name, paste0(Pakka_NM_Dynamic, "<br>", Cards_in_Main, "/", Cards_in_side)]
                column(width = 2, tags$h4(style = "color:red", HTML(deck_header)), uiOutput(deck_name, style = "min-height:100px;background-color:grey;overscroll-behavior-y: contain; touch-action: none;"))
              })
            ),
              dragula(c("Drafted_cards_column",  "first_picks", mydecks), id = "drag_cards_to_deck"),
              fluidRow(column(offset = 9, width = 3, actionButton("toggle_saving", label = "Save button on/off"),
                              hidden(actionButton("save_drafts_to_decks", label= "Save drafted cards to decks"))))

)





})


output$Drafted_cards_column <- renderUI({
#dtc <- data.table( mtcars)
#  lapply(paste0("card_", dtc[, mpg]), function(nm) tags$h3(drag = nm, nm))
  req(input$todo_Drafts)
  print("kuvat pitäs päivittyy")
  input$todo_Drafts
  values$lastUpdated


  uudet_kortit <- ReactDraftCards_d2d$image_ids[PICK_ORDER > 2]

  uudet_kortit[, DRAFT_GROUP := .GRP, by = DRAFT_ID]
  uudet_kortit[, kortteja_pussissa := .N + 1, by = DRAFT_GROUP]
  lapply(paste0("card_", uudet_kortit[, MID]), function(nm) tags$h3(drag = nm, nm))

  for (i in 1:nrow(uudet_kortit)) {
     local({

      my_i <- i
      image_id <- uudet_kortit[i, image_id]
      draft_group <- uudet_kortit[i, DRAFT_GROUP]
      kortit_pussissa <- uudet_kortit[i, kortteja_pussissa]

      image_nm <- paste0(uudet_kortit[i, MID], "_card_small.jpg")

      image_output_name_d2d <- paste0(image_id, "_d2d")
      peruslandi <- image_read(paste0("./www/", image_nm))
    #  browser()

      new_land <- image_annotate(peruslandi, paste0(draft_group, "-", kortit_pussissa), gravity = "north", size = 30, color = "red")
      new_card_folder <- paste0("./www/", uudet_kortit[i, MID], "_", draft_group, "_card.jpg")
      image_write(new_land, new_card_folder, format = "jpg")
      #final_land <- image_annotate(new_land, 3, gravity = "northeast", size = 14, color = "black", location ="+10+8")
      output[[image_output_name_d2d]] <-  renderImage({

        # output[[image_id]] <-  renderImage({
        # list(src = paste0("./www/",image_nm),#image_nm,
        #      alt = "Image failed to render"
        # )
        list(src = new_card_folder,#image_nm,
             alt = "Image failed to render"
        )
      }, deleteFile = TRUE)
    })
  }

  #piirretään vaan jäljellä olevat
  jaljella_olevat <- ReactDraftCards_d2d$cards_left[PICK_ORDER > 2]



  # fluidRow(
  #   column(width = 11, offset = 1,
           lapply( 1:nrow(jaljella_olevat), function(x) {
             MIDi_d2d <- paste0(jaljella_olevat[x, image_id], "_d2d")
             tags$h3(tags$div(imageOutput(MIDi_d2d,

                                  height = "40px",
                                  width = "100%"

                                  #  hover = hoverOpts(id = nimi)
             ),
             Width = "100%"), drag = MIDi_d2d


           )})
 #   )
 # )

})


output$first_picks <- renderUI({
  #dtc <- data.table( mtcars)
  #  lapply(paste0("card_", dtc[, mpg]), function(nm) tags$h3(drag = nm, nm))
  req(input$todo_Drafts)
  print("kuvat pitäs päivittyy")
  input$todo_Drafts
  values$lastUpdated


  uudet_kortit <- ReactDraftCards_d2d$image_ids[PICK_ORDER <= 2]
  uudet_kortit[, DRAFT_GROUP := .GRP, by = DRAFT_ID]
  uudet_kortit[, kortteja_pussissa := .N + 1, by = DRAFT_GROUP]
  lapply(paste0("card_", uudet_kortit[, MID]), function(nm) tags$h3(drag = nm, nm))

  for (i in 1:nrow(uudet_kortit)) {

    local({

      my_i <- i
      image_id <- uudet_kortit[i, image_id]
      draft_group <- uudet_kortit[i, DRAFT_GROUP]
      kortit_pussissa <- uudet_kortit[i, kortteja_pussissa]

      image_nm <- paste0(uudet_kortit[i, MID], "_card_small.jpg")

      image_output_name_d2d <- paste0(image_id, "_d2d")
      peruslandi <- image_read(paste0("./www/", image_nm))
      new_land <- image_annotate(peruslandi, paste0(draft_group), gravity = "north", size = 30, color = "red")
      new_card_folder <- paste0("./www/", uudet_kortit[i, MID], "_", draft_group, "_card.jpg")
      image_write(new_land, new_card_folder, format = "jpg")
      output[[image_output_name_d2d]] <-  renderImage({

        # output[[image_id]] <-  renderImage({
        list(src = new_card_folder,#paste0("./www/",image_nm),#image_nm,
             alt = "Image failed to render"
        )
      }, deleteFile = FALSE)
    })
  }

  #piirretään vaan jäljellä olevat
  jaljella_olevat <- ReactDraftCards_d2d$cards_left[PICK_ORDER <= 2]

  # fluidRow(
  #   column(width = 11, offset = 1,
  lapply( 1:nrow(jaljella_olevat), function(x) {
    MIDi_d2d <- paste0(jaljella_olevat[x, image_id], "_d2d")
    tags$h3(tags$div(imageOutput(MIDi_d2d,

                                 height = "40px",
                                 width = "100%"

                                 #  hover = hoverOpts(id = nimi)
    ),
    Width = "100%"), drag = MIDi_d2d


    )})
  #   )
  # )

})

observeEvent(input$drag_cards_to_deck, {
#  browser()
  print(dragulaValue(input$drag_cards_to_deck))
})


ReactDraftCards_d2d <- reactiveValues(image_ids = NULL,
                                  cards_left = NULL)

observeEvent(input$todo_Drafts,{
  req(input$todo_Drafts)
  req( omistaja_ID_calc$value)

  uudet_kortit <- dbQ(paste0("SELECT MID, PICK_ORDER, id as DRAFT_CARDS_ID, DRAFT_ID
                              FROM DRAFT_CARDS
                             WHERE
                             PICKED = 0 AND
                             OMISTAJA_ID = \"", omistaja_ID_calc$value, "\""),
                      con)


  #tee uniikit imageIdt

  uudet_kortit[, image_id := paste0("DraftBar", seq_len(.N))]
  ReactDraftCards_d2d$image_ids <- uudet_kortit[, .(MID, image_id, DRAFT_CARDS_ID, PICK_ORDER, DRAFT_ID)]
  ReactDraftCards_d2d$cards_left <- uudet_kortit[, .(MID, image_id, DRAFT_CARDS_ID, PICK_ORDER, DRAFT_ID)]

}, ignoreNULL = TRUE, ignoreInit = TRUE)



