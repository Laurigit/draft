#load("../mstat2/external_files/MANASTACK_CARDS.RData")



required_data("ADM_VISUALIZE_CARDS")



output$boxes <- renderUI({

  sata <- ADM_VISUALIZE_CARDS[Pakka_form_ID == 70, .(Converted_Cost, Name, colOrder, Rarity, Maindeck)]

  lapply(sata[, Name], function(x) {
    getCardImg_full(x)
  })

  sorted <- sata[order(Converted_Cost, Name)][Rarity != "Basic Land" & Maindeck ==  TRUE]#[Converted_Cost > 0 & Converted_Cost < 7]
  side <- sata[order(Converted_Cost, Name)][Rarity != "Basic Land" & Maindeck ==  FALSE]
  #add basic lands
  blands <- sata[Rarity == "Basic Land", .N, by = Name][order(-N)]
  land_rows <- data.table(Name = blands[, Name], Converted_Cost = NA, colOrder = -1, Rarity = "Basic Land", Maindeck = TRUE)

  add_basics <- rbind(sorted, land_rows)

  #laske jarjestyes
  add_basics[, order_No := seq_len(.N), by = Converted_Cost]
  max_order_no <- add_basics[, max(order_No)]
  side[, order_No := seq_len(.N) + max_order_no, by = Converted_Cost]


  kaadettu_main <- dcast.data.table(add_basics, formula =   order_No ~ colOrder, value.var = "Name" )[, Maindeck := TRUE]
kaadettu_side <-  dcast.data.table(side, formula =   order_No ~ colOrder, value.var = "Name" )[, Maindeck := FALSE]
kaadettu <- rbind(kaadettu_main, kaadettu_side, fill = TRUE)
#append side ja maini

  columnWidth <- 1

  max_cc <- sata[!is.na(Converted_Cost), max(Converted_Cost)]
  print(max_cc)
  max_kortit <- kaadettu[, max(order_No)]
  print(max_kortit)
  boxno <- 0
  offset_laskenta <- kaadettu
  offset_laskenta[is.na(offset_laskenta)] <- columnWidth
  offset_laskenta_sscol <- offset_laskenta[,2:(max_cc + 1)]
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
      nimi <- kaadettu[order_No == i, j + 1, with = FALSE]

      #check if basic land
      if (nimi %in% c("Mountain", "Forest", "Swamp", "Plains", "Island")) {
        value_input <-  blands[Name == nimi, N]
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
               box(title = NULL,
                   paste0(value_input, " ", nimi),
                 background = land_color,
                width = NULL,
            collapsible = FALSE,
            height = "40px"
                ))
       # HTML('<div id = "logo"><h4>paste0(nimi, " ", value_input)</h4>, background = land_color </div>')

      } else {

      sarake <- max(j, 1)
      offsetti <- as.numeric(convToNumOffset[rivi == i, sarake, with = FALSE])
      print(nimi)
       print(paste0("i", i))
      print(paste0("j", j))

      print(offset_tulos[rivi == i])
      if (offsetti == 1 ) {
    #    if(i == 7)  {print("7")}
        offset_counter <<- offset_counter + 1
      } else {
          reset_next_round <<- TRUE
        }

      print(paste0("offset_counter", offset_counter))

      if (is.na(nimi)) {

        # print(paste0("i", i))
        # print(paste0("j", j))
        # print(offset_tulos[rivi == i])

        ""

      } else {

        column(width = columnWidth,
               offset = offset_counter,
               HTML(paste0('<div id="logo"><img src= "', nimi, '_card.jpg"> </div>')))
       # column(width = 2, offset = offsetti,     box(id = paste0("box ", boxno),  HTML('<img src="Kitchen Finks_card.jpg">')))
       # column(width = 2, offset = offsetti,     HTML('<div id="logo" style="background:url(Kitchen Finks_card.jpg)"></div>'))


        }

}
    })

  )
})

#   for (sarake in 1:max_cc) {
#     boxno <-  boxno + 1
# print(boxno)
#
#   }





})
