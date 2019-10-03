#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(clipr)
library(shinydashboard)



user_logged <- reactiveValues(count = 0)
omistaja_ID_calc <- reactiveValues(value = NULL)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  func_login <- function(input_user_count, clientDataInput) {
    cdata <- clientDataInput
    login <- cdata[["url_search"]]

    nimi <- word(login, 2, sep = "=")
    print("nimi")
    print(login)
    print(nimi)
    if (login == "") {
      if (input_user_count == 1) {
        result <- "Lauri"
      } else {
        result <- "Martti"
      }
    } else {
      result <- nimi
    }
    return(result)
  }
  isolate(user_logged$count <- user_logged$count + 1)
  session$user <- isolate(func_login(user_logged$count, session$clientData))
  omistaja_ID_calc$value <- ifelse(session$user == "Martti", "M", "L")



  sourcelist <- data.table(polku = c(dir("./scripts/", recursive = TRUE)))
  sourcelist[, rivi := seq_len(.N)]
  suppressWarnings(sourcelist[, kansio := strsplit(polku, split = "/")[[1]][1], by = rivi])
  sourcelist <- sourcelist[!grep("load_scripts.R", polku)]
  sourcelist[, kansio := ifelse(str_sub(kansio, -2, -1) == ".R", "root", kansio)]

  input_kansio_list <- c(

    "tab",
    "root"
  )
  for(input_kansio in input_kansio_list) {
    dir_list <- sourcelist[kansio == input_kansio, polku]
    for(filename in dir_list) {
      result = tryCatch({
        print(paste0("sourcing at server", filename))
        source(paste0("./scripts/", filename), local = TRUE)
        print(paste0("sourced at server", filename))
      }, error = function(e) {
        print(paste0("error in loading file: ", filename))
      })
    }
  }




  values <- reactiveValues(
    lastUpdated = "NULL",
    lastAccepted = "NULL"
  )

  deck_changes <- reactiveValues(draft = data.table(MID = numeric(),
                                                    Pakka_ID = numeric(),
                                                    DRAFT_CARDS_ID = numeric()))

  observeEvent( values$lastUpdated, {
# täällä seurataan tuplaklikkauksia ja hoidetaan niitten seurauksia.
    print("TOIMIII")
    req(input$myDecks)
    req(ReactDraftCards$cards_left)
    req(input[[values$lastUpdated]])
    print(input[[values$lastUpdated]] )
    if (input[[values$lastUpdated]]$x >= 0) {
    print("TOIMIII2")

    removed_image_id <- values$lastUpdated
    changed_MID <- ReactDraftCards$image_ids[image_id == values$lastUpdated, MID]
    draft_card_id <- ReactDraftCards$image_ids[image_id == values$lastUpdated, DRAFT_CARDS_ID]
    new_row <- isolate(data.table(MID = changed_MID, Pakka_ID = input$myDecks, DRAFT_CARDS_ID = draft_card_id))
    total_data <- isolate(rbind(deck_changes$draft, new_row))
    deck_changes$draft <- total_data
    #delete from draft

    ReactDraftCards$cards_left <- isolate( ReactDraftCards$cards_left[image_id != removed_image_id])
    }
  })


  observe({
    req(ReactDraftCards$image_ids)
    #input <- NULL
   # input <- c("eka", "toka", "img1", "img3332")
    #imagelist <- names(input)
   # imagelist <- ADM_VISUALIZE_CARDS[Pakka_form_ID == input$pif, image_id]

    imagelist <-   ReactDraftCards$image_ids[, image_id]
    print("IMAGLIST")
    print( imagelist)
  #  print(ADM_VISUALIZE_CARDS[image_id %in% imagelist, Name])
    #imagelist_filtered <- imagelist[str_sub(imagelist,1, 3)  == "img"]
    #imagelist_filtered_no_prev <- imagelist_filtered[! imagelist_filtered %in%  values$lastAccepted]
    lapply(imagelist, function(x) {
      observe({
        input[[x]]
     #   print(ADM_VISUALIZE_CARDS[image_id %in% x, Name])
       # print(str(input[[x]]))
        values$lastUpdated <- x
      })
    })
    #isolate(values$lastAccepted <-  values$lastUpdated)

  })

  output$show_last <- renderPrint({
    req(ReactDraftCards$image_ids)
    req( values$lastUpdated)
    print("rivimaara")
    print(nrow(ReactDraftCards$image_ids))

    if (nrow(ReactDraftCards$image_ids) > 0){

        updated_MID <- ReactDraftCards$image_ids[image_id ==  values$lastUpdated, .(MID)]
        if (nrow(updated_MID) > 0) {
           #STG_CARDS_DIM[MID == updated_MID[, MID]  , Name]
            cardNames <- STG_CARDS_DIM[, .(MID = as.numeric(MID), Name)]
           DeckNames <- STG_DECKS_DIM[, .(Pakka_ID, Nimi)]
           if (nrow( deck_changes$draft) > 0) {
           raw_data <- deck_changes$draft[, .(MID = as.numeric(MID), Pakka_ID = as.numeric(Pakka_ID))]
           joinaa <- cardNames[raw_data, on = "MID"]
           joinaa_pid <- DeckNames[joinaa, on = "Pakka_ID"]

           print(joinaa_pid[order(Pakka_ID), .(Nimi, Name)])
           }
        }
    }

  #  str(input[[values$lastUpdated]])

  })



}


