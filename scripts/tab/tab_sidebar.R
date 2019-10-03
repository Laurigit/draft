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
              choices = mun_draftatyt[, DRAFT_ID])


})


#näytä imaget sidebarissa
ReactDraftCards <- reactiveValues(image_ids = NULL,
                                  cards_left = NULL)

observeEvent(input$todo_Drafts,{
  uudet_kortit <- dbQ(paste0("SELECT MID
                              FROM DRAFT_CARDS
                             WHERE DRAFT_ID = ", input$todo_Drafts, " AND
                             PICKED = 0 AND
                             OMISTAJA_ID = \"", omistaja_ID_calc$value, "\""),
                      con)


  #tee uniikit imageIdt

  uudet_kortit[, image_id := paste0("DraftBar", seq_len(.N))]
  ReactDraftCards$image_ids <- uudet_kortit[, .(MID, image_id)]
  ReactDraftCards$cards_left <- uudet_kortit[, .(MID, image_id)]

})


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
