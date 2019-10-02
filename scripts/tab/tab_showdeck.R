#load("../mstat2/external_files/MANASTACK_CARDS.RData")



required_data("ADM_VISUALIZE_CARDS")

#tee ekana korteista idt

observe({
  if(is.null(input$sb)) return(NULL)




  kortit <-  ADM_VISUALIZE_CARDS[Pakka_form_ID == input$pif, .(Converted_Cost, Name, colOrder, Rarity, Maindeck, image_id, MID)]
  print("KORTIT")
  for (i in 1:nrow(kortit))
   # for (i in 1:1)
  {

    local({
      my_i <- i
      image_id <- kortit[i, image_id]
     # print(image_id)
      image_nm <- paste0(kortit[i, MID], "_card.jpg")
     # print(image_nm)
      output[[image_id]] <-  renderImage({

        # output[[image_id]] <-  renderImage({
          list(src = paste0("./www/",image_nm),#image_nm,
               alt = "Image failed to render"
               )
        }, deleteFile = FALSE)
    })
  }
})

get_sorted_cards <- function(ADM_VISUALIZE_CARDS, input_Pakka_form_ID = 257) {
  sata <- ADM_VISUALIZE_CARDS[Pakka_form_ID == input_Pakka_form_ID, .(Converted_Cost, Name, colOrder, is_basic_land, Maindeck, image_id, MID)]

  lapply(sata[, MID], function(x) {

    getCardImg_full(x)
  })

  sorted <- sata[order(Converted_Cost, Name)][is_basic_land == FALSE & Maindeck ==  TRUE]#[Converted_Cost > 0 & Converted_Cost < 7]
  side <- sata[order(Converted_Cost, Name)][is_basic_land == FALSE & Maindeck ==  FALSE]
  #add basic lands
  blands <- sata[is_basic_land == TRUE, .N, by = Name][order(-N)]
  if (nrow(blands) > 0) {
  land_rows <- data.table(Name = blands[, Name], Converted_Cost = NA, colOrder = -1, is_basic_land = TRUE, Maindeck = TRUE, image_id = blands[, Name], MID = blands[, Name])
  }
  add_basics <- rbind(land_rows, sorted)

  #laske jarjestyes
  add_basics[, order_No := seq_len(.N), by = Converted_Cost]
  max_order_no <- add_basics[, max(order_No)]
  side[, order_No := seq_len(.N) + max_order_no, by = Converted_Cost]


  kaadettu_main_ids <- dcast.data.table(add_basics, formula =   order_No ~ colOrder, value.var = "image_id" )

  #tarkista eka, etta sideissa on kortteja
   if(nrow(side) > 0){
  kaadettu_side_ids <-  dcast.data.table(side, formula =   order_No ~ colOrder, value.var = "image_id" )
  kaadettu_side_nimi <-  dcast.data.table(side, formula =   order_No ~ colOrder, value.var = "Name" )
  }
  #append side ja maini ja sidevälirivi
  siderivi <- data.table("-1" = "Side")
  kaadettu_ids <- rbind(kaadettu_main_ids, siderivi, kaadettu_side_ids, fill = TRUE)[, order_No := NULL]
  kaadettu_ids[, order_No := seq_len(.N)]



  kaadettu_main_nimi <- dcast.data.table(add_basics, formula =   order_No ~ colOrder, value.var = "Name" )

  #append side ja maini
  kaadettu_nimi <- rbind(kaadettu_main_nimi, kaadettu_side_nimi, fill = TRUE)[, order_No := NULL]
  kaadettu_nimi[, order_No := seq_len(.N)]
  reslist <- NULL
  reslist$nimi <- kaadettu_nimi
  reslist$id <- kaadettu_ids
  reslist$maxcc <- sata[!is.na(Converted_Cost), max(Converted_Cost)]
  reslist$blands <- blands
  reslist$last_main_row <- max_order_no
  return(reslist)
}



output$boxes <- renderUI({
  # input <- NULL
  # input$pfi <- 72
 # kaadettu_all <-  get_sorted_cards(ADM_VISUALIZE_CARDS, 129)
 kaadettu_all <-  get_sorted_cards(ADM_VISUALIZE_CARDS, input$pif)
 kaadettu <- kaadettu_all$id
print( get_sorted_cards(ADM_VISUALIZE_CARDS, input$pif)$nimi)
print(input$pif)
print("BOXES")
  columnWidth <- 1

  max_cc <- kaadettu_all$maxcc
  print(max_cc)
  max_kortit <- kaadettu[, max(order_No)]
  print(max_kortit)
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

  offset_counter<- 0
  reset_next_round <- FALSE
lapply(1:max_kortit, function(i) {
  offset_counter <<- 0
  reset_next_round <<- FALSE

  fluidRow(
    lapply(1:max_cc, function(j){
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
      } else {
        main_height <- "60px"
        side_height <- "60px"
        showLands <- TRUE
      }
      #check if basic land
      if (nimi %in% c("Mountain", "Forest", "Swamp", "Plains", "Island"))  {
        value_input <-  kaadettu_all$blands[Name == nimi, N]
        if (nimi == "Island") {
            land_color <- "blue"
        } else if ((nimi == "Forest")) {
          land_color <- "green"
        }else if ((nimi == "Plains")) {
          land_color <- "yellow"
        }else if ((nimi == "Swamp")) {
          land_color <- "black"
        }else if ((nimi == "Mountain")) {
          land_color <- "red"
        }

        column(width = columnWidth,
              if ( showLands == TRUE) {
               box(title = NULL,
                   paste0(value_input, " ", nimi),
                 background = land_color,
                width = NULL,
            collapsible = FALSE,
            height = "40px"
                )
              } else {
          HTML("")
        }
          )
       # HTML('<div id = "logo"><h4>paste0(nimi, " ", value_input)</h4>, background = land_color </div>')
      } else if (nimi %in% "Side") {
        print("else if")
        column(width = 12,

                 box(title = NULL,
                    "Sideboard",
                     background = "blue",
                     width = NULL,
                     collapsible = FALSE,
                     height = "150px"
                 ))
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
