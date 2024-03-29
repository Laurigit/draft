#load("../mstat2/external_files/MANASTACK_CARDS.RData")

output$deck_stats <- renderText({
req(input$myDecks)
  required_data(c("STG_CARDS", "STG_CARDS_DIM", "STG_DECKS_DIM"))
  con <- connDB(con)
  stat_pfi <- dbSelectAll("STAT_PFI", con)
  if (!isRunning()) {
    input <- NULL
    input$myDecks <- 3
  }
  pfi <- STG_CARDS[Pakka_ID == input$myDecks, max(Pakka_form_ID)]
 res <- count_deck_stats(pfi, STG_CARDS, STG_CARDS_DIM)
 pakka_NM <- STG_DECKS_DIM[Pakka_ID == input$myDecks, Nimi]
 PFI_card_count <- floor(stat_pfi[Deck == pakka_NM, Deck_size] + 40)
 paste0("Required cards = ", PFI_card_count,
        " Total cards = ", res$card_count,
        " Land% = ", paste0(round(as.numeric(res$land_count) / as.numeric(res$card_count), 3) * 100), "%",
        " Manasymbol% = ", res$mana_costs,
        " Creature% = ", paste0(round(as.numeric(res$creature_count) / as.numeric(res$card_count), 3) * 100), "%",
        " Ins_sorc%% = ", paste0(round(as.numeric(res$inc_or_sorc_count) / as.numeric(res$card_count), 3) * 100), "%"
        )
})

required_data(c("ADM_VISUALIZE_CARDS", "ADM_LAND_IMAGES"))
output$sideboard_bar <- renderImage({

  list(src = paste0("./external_files/sideboard.JPG"),#image_nm,
       alt = "Image failed to render"
  )
}, deleteFile = FALSE)

output$deck_selector <- renderUI({
  required_data("STG_DECKS_DIM")
print("output$deck_selector ")
  # session <- NULL
  # session$user <- "Lauri"
  my_decks <- STG_DECKS_DIM[Omistaja_NM == session$user & !(Retired == 1 & Side == 0) & Side != -1, .(Pakka_ID, Side, Nimi)]
  sorted <- my_decks[order(Side, Pakka_ID)]
  radioButtons(inputId = "myDecks",
               label = NULL,
               choiceNames = sorted[, Nimi],
               choiceValues = sorted[, Pakka_ID],
               inline = TRUE)
})


#tee ekana korteista idt


observe({

  #req(session$user)
  #DEPPEN DONT DEL
  local_update_data$update
  #DEPPEN DONT DEL
required_data("STG_DECKS_DIM")
  required_data("ADM_VISUALIZE_CARDS")
  #create dep ÄLÄ TUHOA
  #main <- NULL
  #side <- NULL

  main$cards <- ADM_VISUALIZE_CARDS[Maindeck == 1 & NineSide == 0  , .(image_id, MID, Pakka_ID)]
  side$cards <- ADM_VISUALIZE_CARDS[Maindeck == 0 & NineSide == 0 , .(image_id, MID, Pakka_ID)]
#  session <- NULL
 # session$user <- "Lauri"
  my_current_pakkaids <- STG_DECKS_DIM[Omistaja_NM == session$user  & !(Retired == 1 & Side == 0) & Side != -1, Pakka_ID]
  my_current_pfis <- ADM_VISUALIZE_CARDS[Pakka_ID %in% my_current_pakkaids, max(Pakka_form_ID), by = Pakka_ID]
  kortit <-  ADM_VISUALIZE_CARDS[  Pakka_form_ID %in% my_current_pfis[, V1], .(image_file, Pakka_ID, Converted_Cost, Name, colOrder, Rarity, Maindeck, image_id, MID)]

  #tähän vois laittaa, että ysisidejä ei vielä valmistauduta piirtään.

  #create land image files
  print("KORTIT")

  for (i in 1:nrow(kortit))
   # for (i in 1:1)
  {

    local({
      my_i <- i
     # print(paste0(kortit[i, image_id], " ", kortit[i, image_file], " ", kortit[i, Name]))
      image_id <- kortit[i, image_id]
      image_filu <- kortit[i, image_file]

      output[[image_id]] <-  renderImage({

        # output[[image_id]] <-  renderImage({
          list(src = paste0("./www/",image_filu),#image_nm,
               alt = "Image failed to render"
               )
        }, deleteFile = FALSE)
    })
  }
})

observeEvent(input$save_changes_button,{
  #function(new_DCIDs, removed_DCIDs, Pakka_ID_input, STG_CARDS, STG_CARDS_DIM, STG_DRAFT_CARDS)

  #  new_row <- isolate(data.table(source = paste0("Main_", input$session, "_", input$myDecks),
  # MID = changed_MID,
  # Pakka_ID = input$myDecks,
  # DRAFT_CARDS_ID = draft_card_id,
  # Maindeck = -1,
  # Removed_from_game = TRUE))
  required_data(c("STG_CARDS",
                  "STG_CARDS_DIM",
                  "STG_DRAFT_CARDS"
                  ))

  new_DCIDs <- deck$changes[Pakka_ID == input$myDecks & source == "Side", .(DRAFT_CARDS_ID, MID)]
  removed_DCIDs <- deck$changes[Pakka_ID == input$myDecks & source == "Main", DRAFT_CARDS_ID]
  Pakka_ID_input <- input$myDecks
  con <- connDB(con)
  new_dl <-  createNewDecklist_after_changes(new_DCIDs,
                                  removed_DCIDs,
                                  Pakka_ID_input,
                                  STG_CARDS,
                                  STG_CARDS_DIM,
                                  STG_DRAFT_CARDS,
                                  con)
  new_dl[, Valid_from_DT := now(tz = "EET")]
  #browser()
  dbWriteTable(con, "CARDS", new_dl, row.names = FALSE, append = TRUE)
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_CARDS", ADM_DI_HIERARKIA, input_env = globalenv())
  deck$changes <- deck$changes[1 == 0]
})



output$boxes <- renderUI({

  req(input$myDecks)

  #create dependency
  #create dep ÄLÄ TUHOA
  input$save_changes_button
  input$saveDraftedCards
  #DEP VALMIS
  print("output$boxes")
  # input <- NULL
  # input$pfi <- 72
  #input$myDecks <- 43
  #input$Card_age_selector <-40
 # kaadettu_all <-  get_sorted_cards(ADM_VISUALIZE_CARDS, 129)
  show_pfi <- STG_CARDS[Pakka_ID == input$myDecks, max(Pakka_form_ID)]

  draw_deck_pfi_all <- ADM_VISUALIZE_CARDS[Pakka_form_ID == show_pfi]
#apply age filter

  if (input$Card_age_selector == "All") {
    draw_deck_pfi <- draw_deck_pfi_all
  } else {

    draw_deck_pfi <- draw_deck_pfi_all[Card_age >= as.numeric(input$Card_age_selector)]
  }


 kaadettu_all <-  get_sorted_cards(draw_deck_pfi)
 kaadettu <- kaadettu_all$id
#print( get_sorted_cards(ADM_VISUALIZE_CARDS, show_pfi)$nimi)

#print("BOXES")
  columnWidth <- 1

  max_cc <- kaadettu_all$maxcc

  max_kortit <- kaadettu[, max(order_No)]

  boxno <- 0
  offset_laskenta <- kaadettu

  offset_laskenta[is.na(offset_laskenta)] <- columnWidth
  offset_laskenta_sscol <- offset_laskenta[,1:(max_cc)]
  convToNumOffset <- as.data.table(lapply(offset_laskenta_sscol, as.numeric))
  convToNumOffset[is.na(convToNumOffset)] <- 0
  convToNumOffset[, rivi := seq_len(.N)]
  #offset_tulos <- data.table(t(apply(convToNumOffset, 1, cumsum)))
  #offset_tulos <- as.data.table(convToNumOffset)
 # offset_tulos[is.na(offset_tulos)] <- 0
 # offset_tulos[, rivi := seq_len(.N)]

  offset_counter <- 0
  reset_next_round <- FALSE

  lapply(1:max_kortit, function(i) {
  offset_counter <<- 0
  reset_next_round <<- FALSE

  fluidRow(
    lapply(1:max_cc, function(j) {
      if (reset_next_round == TRUE) {
        offset_counter <<- 0
        reset_next_round <<- FALSE
      }
      nimi <- kaadettu[order_No == i, j, with = FALSE]

       if(input$main_side == "Main") {
        main_height <- "60px"
        side_height <- "20px"
        showLands <- TRUE
      } else if (input$main_side == "Side") {
        main_height <- "20px"
        side_height <- "60px"
        showLands <- FALSE
      } else if (input$main_side == "Neither") {
        main_height <- "20px"
        side_height <- "20px"
        showLands <- FALSE
      } else {
        main_height <- "60px"
        side_height <- "60px"
        showLands <- TRUE
      }

      if (nimi %in% "Side") {
        column(width = 12, imageOutput("sideboard_bar", width = "1600px", height = "70px"))
        # print("else if")
        # column(width = 12,
        #
        #          box(title = NULL,
        #             "Sideboard",
        #              background = "blue",
        #              width = NULL,
        #              collapsible = FALSE,
        #              height = "150px"
        #          ))
      } else {

      sarake <- max(j, 1)
      offsetti <- as.numeric(convToNumOffset[rivi == i, sarake, with = FALSE])
      #print(paste0("i ", i, "j ", j, "offset ", offsetti))
      # print(nimi)
      #  print(paste0("i", i))
      # print(paste0("j", j))
      #
      # print(offset_tulos[rivi == i])
      if (offsetti == 1 ) {
    #    if(i == 7)  {print("7")}
        offset_counter <<- offset_counter + 1
      } else {
          reset_next_round <<- TRUE
        }

  #    print(paste0("offset_counter", offset_counter))

      if (is.na(nimi)) {

        # print(paste0("i", i))
        # print(paste0("j", j))
        # print(offset_tulos[rivi == i])

        ""

      } else {
        if (i > kaadettu_all$last_main_row) {
          row_heigh <- side_height
        } else {
          row_heigh <- main_height
        }
#if (i == 15 & j == 8) { browser()}
        column(width = columnWidth,
               offset = offset_counter,
              # HTML(paste0('<div id="logo"><img src= "', nimi, '_card.jpg"> </div>'))
               imageOutput(nimi,
                           height = row_heigh,
                           dblclick = dblclickOpts(id = nimi)
                         #  hover = hoverOpts(id = nimi)
                           )
               )

       # column(width = 2, offset = offsetti,     box(id = paste0("box ", boxno),  HTML('<img src="Kitchen Finks_card.jpg">')))
       # column(width = 2, offset = offsetti,     HTML('<div id="logo" style="background:url(Kitchen Finks_card.jpg)"></div>'))


        }

}
    })

  )


})



})


# # #basic land handling
#
observeEvent(input$add_basic_land,{

  print("HEII")
    req(input$basic_land)
    req(deck$changes)

    changed_MID <- input$basic_land

    #get free DCID

    #check first if lands are already added
    min_id <- deck$changes[basic_land == TRUE, min(DRAFT_CARDS_ID)]
    #if min id is positive then this is first new land
    first_land <- ifelse(min_id < 0, FALSE, TRUE)
    #get free DCID based on the result
    if (first_land == TRUE) {
      input_draft_cards_id  <- (STG_CARDS[, min(DRAFT_CARDS_ID)]) - 1
    } else {
      input_draft_cards_id  <- (deck$changes[, min(DRAFT_CARDS_ID)]) - 1
    }
    new_row <- isolate(data.table(source = paste0("Side"),
                                  MID = changed_MID,
                                  Pakka_ID = input$myDecks,
                                  DRAFT_CARDS_ID = input_draft_cards_id,
                                  Maindeck = 1,
                                  Removed_from_game = FALSE,
                                  basic_land = TRUE))
    print("Ennen ländiä")
    print(deck$changes )
    deck$changes <- isolate(rbind(deck$changes, new_row))
    print("Jälkeen")
},
ignoreInit = TRUE, ignoreNULL = TRUE)



observeEvent(input$remove_basic_land,{

  print("remove")
  req(input$remove_basic_land)
  req(deck$changes)

  changed_MID <- input$basic_land
  #get free DCID
  #check first if lands are already added. This works because only basic lands get new negative DCIDs
  min_id <- deck$changes[MID == changed_MID & source == "Side", min(DRAFT_CARDS_ID)]
  #if min id is positive then this is first new land
  first_land <- ifelse(min_id < 0, FALSE, TRUE)

  if (first_land == TRUE) {
   #remove from old list
    #get a correct DCID from old list
    old_PFI <- STG_CARDS[Pakka_ID == input$myDecks, max(Pakka_form_ID)]
    old_dl <- STG_CARDS[Pakka_form_ID == old_PFI, .(MID, Pakka_ID, Name, Maindeck, DRAFT_CARDS_ID)]

    lands_removed_already_from_main <- deck$changes[MID == changed_MID &
                                                      Removed_from_game == TRUE &
                                                      source == "Main",
                                                    DRAFT_CARDS_ID]
    #remove  one of the not already removed lands :)

    removed_DCID <- min(setdiff(old_dl[MID == changed_MID, DRAFT_CARDS_ID],
                            lands_removed_already_from_main))

    new_row <- isolate(data.table(source = paste0("Main"),
                                  MID = changed_MID,
                                  Pakka_ID = input$myDecks,
                                  DRAFT_CARDS_ID = removed_DCID,
                                  Maindeck = -1,
                                  Removed_from_game = TRUE,
                                  basic_land = TRUE))
    deck$changes <- isolate(rbind(deck$changes, new_row))
  } else {
    #remove from temporaryli added cards
    deck$changes <- deck$changes[DRAFT_CARDS_ID != min_id]
  }

  print(deck$changes )

  print("Jälkeen ländiä")

},
ignoreInit = TRUE, ignoreNULL = TRUE)

observeEvent(input$reset_changes,{
  deck$changes <- deck$changes[1 == 0]
})

output$card_age_selector <- renderUI({
  required_data("STAT_SIDE_CARD_AGE")
  req(session$user)

  myAges <- c("All", sort(STAT_SIDE_CARD_AGE()[Omistaja_ID == omistaja_ID_calc$value, .N, by = .(Card_age)][, Card_age]))
  selectInput(inputId = "Card_age_selector", label = "Age_older_than", choices = myAges, selected = "All", multiple = FALSE)
})

#reset
#land pct
