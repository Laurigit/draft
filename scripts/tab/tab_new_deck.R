#tab_new_deck
cards_not_found_from_sides <- reactiveValues(cards = NULL)

observeEvent(input$add_deck,{
con <- connDB(con)

#very first check if the cards are in the sideboard
new_deck_list <- rbind(rv_new_deck$main, rv_new_deck$side)#[1:4]]
new_deck_list[, MID := NULL]
new_deck_list[, Converted_Cost := NULL]
new_deck_list[, Colors := NULL]
#new_deck_list <- new_deck_list[1:4]
#new_deck_list <- data.table(Name = c("Grasp of Phantoms", "Tumble Magnet","Alloy Myr","Freed from the Real","Stangg","Stangg"), monesko_kortti = c(1, 1, 1, 1, 1, 2), Maindeck = c(1,1,1,1,0,0))
required_data(c("STG_CARDS", "STG_DECKS_DIM"))
#session <- NULL
#session$user <- "Lauri"
mysidet <- STG_DECKS_DIM[Omistaja_ID == str_sub(session$user, 1, 1) & Side == 1, Pakka_ID]
my_latest_side_PFI <- STG_CARDS[Pakka_ID %in% mysidet, .(Pakka_form_ID = max(Pakka_form_ID)), by = Pakka_ID]
my_latest_side_decklists <- STG_CARDS[Pakka_form_ID %in% my_latest_side_PFI[, Pakka_form_ID], .(MID, Pakka_ID, DRAFT_CARDS_ID, Name, monesko_kortti)]
#my_latest_side_decklists[monesko_kortti== 2]
#my_latest_side_decklists[MID == 397879]

#get kortit from sides
joini <- my_latest_side_decklists[new_deck_list, on = .(Name, monesko_kortti)]

cards_not_found_from_sides$cards <- joini[is.na(DRAFT_CARDS_ID)]

if (nrow(cards_not_found_from_sides$cards) == 0 ) {


  #first create new deck
required_data("ADM_DI_HIERARKIA")
required_data("SRC_DECKS_DIM")
updateData("SRC_DECKS_DIM", ADM_DI_HIERARKIA, globalenv())

rivi_id <- STG_DECKS_DIM[, max(Pakka_ID)] + 1
#session <- NULL
#session$user <- "Lauri"
# input <- NULL
# input$pakka_nimi <- "newdeck"
free_pakka_no <- as.numeric(STG_DECKS_DIM[Omistaja_NM == session$user, max(Pakka_No)]) + 1


  omistaja_kirain <-  str_sub(session$user, 1, 1)
  omistaja_Nro <- ifelse(omistaja_kirain == "L", 1, 2)

  new_row <- data.table(Nimi = input$pakka_nimi,
                        Divari = 1,
                        rivi_id = rivi_id,
                        Picked = 1,
                        Omistaja = omistaja_Nro,
                        Omistaja_ID = omistaja_kirain,
                        Omistaja_Nimi = session$user,
                        Pakka = free_pakka_no,
                        Manastack_Deck_ID = "",
                        Json_Prefix = paste0(omistaja_kirain, "_", free_pakka_no),
                        Retired = 0,
                        Side = 0,
                        Manastack_name_url = "")
  dbIns("DECKS_DIM", new_row, con)
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_DECKS_DIM", ADM_DI_HIERARKIA, globalenv())




    con <- connDB(con)
    Pakka_ID <-  rivi_id
    #Free_PFI <- 500
    Free_PFI <- as.numeric(dbQ("SELECT MAX(Pakka_form_ID) FROM CARDS", con) + 1)
    new_deck_with_info <- cbind(joini, Pakka_form_ID = Free_PFI, Pakka_ID, CARD_ID = NULL)
    new_deck_with_info[, monesko_kortti := NULL]
    dbWriteTable(con, "CARDS", new_deck_with_info, append = TRUE, row.names = FALSE)
    updateData("SRC_CARDS", ADM_DI_HIERARKIA, globalenv())
    #create new sides
    changed_sides <- new_deck_with_info[, .N, by = Pakka_ID][, Pakka_ID]

    for(update_sides in changed_sides) {
      remove <- new_deck_with_info[Pakka_ID == update_sides, DRAFT_CARDS_ID]
      new_side <- remove_DIDs_from_deck(update_sides, remove, STG_CARDS, con)
      dbWriteTable(con, "CARDS", new_side, append = TRUE, row.names = FALSE)
      updateData("SRC_CARDS", ADM_DI_HIERARKIA, globalenv())
    }

  }

}, ignoreInit = TRUE, ignoreNULL = TRUE)


rv_new_deck <- reactiveValues(main = NULL,
                              side = NULL)

observeEvent(input$load_new_deck_MIDS, {
  #con <- connDB(con)
  #input <- NULL
  #rv_new_deck <- NULL
  #input$new_dect_text_area <- "40001 40002 40003 40001 40001 40002 40005"

  kortti_dt <- readCardsFromTextArea(input$new_deck_text_area, con)
  #kortit <- c(40001, 40002, 40002, 40003)
  required_data("STG_CARDS_DIM")
  #joinaa name


  kortti_dt[, monesko_kortti := seq_len(.N), by = Name]
  if (input$load_to_main_or_side == "Main") {

        rv_new_deck$main <- kortti_dt
        rv_new_deck$main[, Maindeck := 1]

  } else {
        rv_new_deck$side <- kortti_dt
        rv_new_deck$side[, Maindeck := 0]

  }


})


output$show_new_deck <- renderUI({
  req(rv_new_deck$main)

  decki <- rbind(rv_new_deck$main, rv_new_deck$side)

  decki[, image_id := paste0("img_", Name, monesko_kortti, Maindeck)]
  decki[, colOrder := Converted_Cost]
  decki[, is_basic_land := FALSE]
  kaadettu_all <- get_sorted_cards(decki)
  kaadettu <- kaadettu_all$id
  kaadettu_MID <- kaadettu_all$MID
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

  offset_counter_new_deck<- 0
  reset_next_round_new_deck <- FALSE

  lapply(1:max_kortit, function(i) {
    offset_counter_new_deck <<- 0
    reset_next_round_new_deck <<- FALSE

    fluidRow(
      lapply(1:max_cc, function(j){
        if (reset_next_round_new_deck == TRUE) {
          offset_counter_new_deck <<- 0
          reset_next_round_new_deck <<- FALSE
        }
        nimi <- kaadettu[order_No == i, j, with = FALSE]
        MID <- kaadettu_MID[order_No == i, j, with = FALSE]

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

          } else {

          sarake <- max(j, 1)
          offsetti <- as.numeric(convToNumOffset[rivi == i, sarake, with = FALSE])
          # print(nimi)
          #  print(paste0("i", i))
          # print(paste0("j", j))
          #
          # print(offset_tulos[rivi == i])
          if (offsetti == 1 ) {
            #    if(i == 7)  {print("7")}
            offset_counter_new_deck <<- offset_counter_new_deck + 1
          } else {
            reset_next_round_new_deck <<- TRUE
          }

          #    print(paste0("offset_counter_new_deck", offset_counter_new_deck))

          if (is.na(nimi)) {



            ""

          } else {
            if (i > kaadettu_all$last_main_row) {
              row_heigh <- side_height
            } else {
              row_heigh <- main_height
            }

            output[[nimi[[1]]]] <-  renderImage({

              # output[[image_id]] <-  renderImage({
              list(src = paste0("./www/", MID, "_card.jpg"),#image_nm,
                   alt = "Image failed to render"
              )
            }, deleteFile = FALSE)

            column(width = columnWidth,
                   offset = offset_counter_new_deck,
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

