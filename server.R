#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



global_update_data <- reactiveValues(update = 1)

user_logged <- reactiveValues(count = 0)



server <- function(input, output, session) {
  local_update_data <- reactiveValues(update = 1)


  STAT_SIDE_CARD_AGE <- reactivePoll(30000, session, checkFunc = function() {
    con <- connDB(con)
    dbQ("SELECT count(Name) FROM betmtg2.STAT_SIDE_CARD_AGE", con)},
    valueFunc = function() { dbSelectAll("STAT_SIDE_CARD_AGE", con)}
  )
  STAT_CURRENT_PAKKA <- reactivePoll(30000, session, checkFunc = function() {
    con <- connDB(con)
    dbQ("SELECT count(Colors) FROM betmtg2.STAT_CURRENT_PAKKA", con)},
    valueFunc = function() { dbSelectAll("STAT_CURRENT_PAKKA", con)}
  )



  omistaja_ID_calc <- reactiveValues(value = NULL)
  required_data("ADM_CARD_IMAGES")
  #UI SETTINGS
  shinyjs::disable(id = "save_picks")


  #CHECK THAT IS HAVE ALL THE IMAGES LOADED THAT ARE IN CARDS_DIM. ADM_CARD_IMAGES does it
required_data("ADM_DI_HIERARKIA")
updateData("SRC_CARDS_DIM", ADM_DI_HIERARKIA, globalenv(), FALSE)




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
  #omistaja_ID_calc$value <- "M"
print("OMISTAJA_ID_CAL_C CALUE = ")
isolate(print(omistaja_ID_calc$value ))


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

required_data("ADM_DI_HIERARKIA")
  #reactive file to observe which cards are on side and main and recognize the clicks
  main <- reactiveValues(cards = NULL)
  side <- reactiveValues(cards = NULL)


  values <- reactiveValues(
    lastUpdated = "NULL",
    lastAccepted = "NULL"
  )

  deck <- reactiveValues(changes = data.table(source = character(),
                                                    MID = numeric(),
                                                    Pakka_ID = numeric(),
                                                    DRAFT_CARDS_ID = numeric(),
                                                    Maindeck = as.character(),
                                                    Removed_from_game = as.numeric(),
                                              basic_land = as.numeric()))

  observeEvent(values$lastUpdated, {
# täällä seurataan tuplaklikkauksia ja hoidetaan niitten seurauksia.
    print("TOIMIIIsds")
    req(input$myDecks)
   # req(ReactDraftCards$cards_left)
    req(input[[values$lastUpdated]])

    print(values$lastUpdated)
   # print(input[[values$lastUpdated]] )

    #purkkaa, jos ei oo draftattavaa

    if (is.null(ReactDraftCards$image_ids)) {

      ReactDraftCards$image_ids <- isolate(data.table(image_id = -9999999999999999, MID = -99999999999999999999999, DRAFT_CARDS_ID = -99999999999999999))
    }

    if (input[[values$lastUpdated]]$x >= 0) {
    print("TOIMIII2")

      clicked <- ""
      #check where was clicked
      if (nrow(ReactDraftCards$image_ids[image_id ==  values$lastUpdated]) > 0) {
        clicked <- "Draft"
      } else if  (nrow(main$cards[image_id ==  values$lastUpdated]) > 0) {
        clicked <- "Main"
      } else if  (nrow(side$cards[image_id ==  values$lastUpdated]) > 0) {
        clicked <- "Side"
      }

      if (clicked == "Draft") {


        removed_image_id <- values$lastUpdated
        changed_MID <- ReactDraftCards$image_ids[image_id == values$lastUpdated, MID]
        draft_card_id <- ReactDraftCards$image_ids[image_id == values$lastUpdated, DRAFT_CARDS_ID]
        #check if drafted card is going to sideboard or mainboard. It oges to sideboard if picked to deck. Goes to main if picked to 9side

        if (STG_DECKS_DIM[Pakka_ID == input$myDecks, NineSide] == 1) {
          toMainorSide <- 1
        } else {
          toMainorSide <- 0
        }
        new_row <- isolate(data.table(source = paste0("Draft", input$select_draft),
                                      MID = changed_MID, Pakka_ID = input$myDecks,
                                      DRAFT_CARDS_ID = draft_card_id,
                                      Maindeck = toMainorSide,
                                      Removed_from_game = FALSE,
                                      basic_land = FALSE))
        deck$changes <- isolate(rbind(deck$changes, new_row))

        #delete from draftmain

        ReactDraftCards$cards_left <- isolate( ReactDraftCards$cards_left[image_id != removed_image_id])
      } else if (clicked == "Main") {
        print("Main clicked")

        changed_MID <- ADM_VISUALIZE_CARDS[image_id == values$lastUpdated, max(MID)]
        draft_card_id <- ADM_VISUALIZE_CARDS[image_id == values$lastUpdated, max(DRAFT_CARDS_ID)]
        new_row <- isolate(data.table(source = paste0("Main"),
                                      MID = changed_MID,
                                      Pakka_ID = input$myDecks,
                                      DRAFT_CARDS_ID = draft_card_id,
                                      Maindeck = -1,
                                      Removed_from_game = TRUE,
                                      basic_land = FALSE))

        deck$changes <- isolate(rbind(deck$changes, new_row))
        print("Poistettu Mainista tulos")
        print(deck$changes)


      } else if (clicked == "Side") {
        print("Side clicked")

        changed_MID <- ADM_VISUALIZE_CARDS[image_id == values$lastUpdated, max(MID)]
        draft_card_id <- ADM_VISUALIZE_CARDS[image_id == values$lastUpdated, max(DRAFT_CARDS_ID)]
        new_row <- isolate(data.table(source = paste0("Side"),
                                      MID = changed_MID,
                                      Pakka_ID = input$myDecks,
                                      DRAFT_CARDS_ID = draft_card_id,
                                      Maindeck = 1,
                                      Removed_from_game = FALSE,
                                      basic_land = FALSE))
        deck$changes <- isolate(rbind(deck$changes, new_row))
        print("Laitettu sidestä tulos")
        print(deck$changes)
      }






    }
  })


  observe({

    req(main$cards, side$cards)
    #req(ReactDraftCards$image_ids, main$cards, side$cards)
    #input <- NULL
   # input <- c("eka", "toka", "img1", "img3332")
    #imagelist <- names(input)
   # imagelist <- ADM_VISUALIZE_CARDS[Pakka_form_ID == input$pif, image_id]

    print("BEFORE IMAGE LIST")
    mainCards <- main$cards[, image_id]
    sideCards <- side$cards[, image_id]
    if (is.null(ReactDraftCards$image_ids)) {

      imagelist <- c(mainCards, sideCards)
    } else {
      draftCards <-   ReactDraftCards$image_ids[, image_id]

      imagelist <- c(draftCards, mainCards, sideCards)
    }

    print("IMAGLIST")
   # print( imagelist)
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
    print("IMAGLIST valmis")
  })


  eV_show_last <- reactive({
   # req(ReactDraftCards$image_ids)
    req( values$lastUpdated)
    print("rivimaara")
   # print(nrow(ReactDraftCards$image_ids))

    required_data(c("ADM_VISUALIZE_CARDS", "STG_CARDS_DIM", "STG_DECKS_DIM"))

    printChanges(deck$changes, ADM_VISUALIZE_CARDS, STG_CARDS_DIM, STG_DECKS_DIM)

    #  str(input[[values$lastUpdated]])
  })
  output$show_last <- renderUI({
   # HTML()

    HTML('<font size="4" color="white">', eV_show_last(),  '</font>')

  })



}


