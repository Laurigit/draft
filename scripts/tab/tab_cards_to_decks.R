#tab_cards_to_decks


output$Drafted_cards_column <- renderUI({
#dtc <- data.table( mtcars)
#  lapply(paste0("card_", dtc[, mpg]), function(nm) tags$h3(drag = nm, nm))
  req(input$todo_Drafts)
  print("kuvat pitäs päivittyy")
  input$todo_Drafts
  values$lastUpdated


  uudet_kortit <- ReactDraftCards_d2d$image_ids
  lapply(paste0("card_", uudet_kortit[, MID]), function(nm) tags$h3(drag = nm, nm))

  for (i in 1:nrow(uudet_kortit)) {
    #print("sidekortit piirtyy")
    local({
      #print(i)
      my_i <- i
      image_id <- uudet_kortit[i, image_id]
      # print(image_id)
      image_nm <- paste0(uudet_kortit[i, MID], "_card_small.jpg")
      # print(image_nm)
      image_output_name_d2d <- paste0(image_id, "_d2d")
      output[[image_output_name_d2d]] <-  renderImage({

        # output[[image_id]] <-  renderImage({
        list(src = paste0("./www/",image_nm),#image_nm,
             alt = "Image failed to render"
        )
      }, deleteFile = FALSE)
    })
  }

  #piirretään vaan jäljellä olevat
  jaljella_olevat <- ReactDraftCards_d2d$cards_left
  print(jaljella_olevat)

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

  uudet_kortit <- dbQ(paste0("SELECT MID, PICK_ORDER, id as DRAFT_CARDS_ID
                              FROM DRAFT_CARDS
                             WHERE
                             PICKED = 0 AND
                             OMISTAJA_ID = \"", omistaja_ID_calc$value, "\""),
                      con)


  #tee uniikit imageIdt

  uudet_kortit[, image_id := paste0("DraftBar", seq_len(.N))]
  ReactDraftCards_d2d$image_ids <- uudet_kortit[, .(MID, image_id, DRAFT_CARDS_ID, PICK_ORDER)]
  ReactDraftCards_d2d$cards_left <- uudet_kortit[, .(MID, image_id, DRAFT_CARDS_ID, PICK_ORDER)]

}, ignoreNULL = TRUE, ignoreInit = TRUE)



