get_sorted_cards <- function(ADM_VISUALIZE_CARDS, input_Pakka_form_ID = 439) {

  sata <- ADM_VISUALIZE_CARDS[Pakka_form_ID == input_Pakka_form_ID, .(Converted_Cost, Name, colOrder, is_basic_land, Maindeck, image_id, MID)]

  lapply(sata[, MID], function(x) {

    getCardImg_full(x)
  })

  sorted <- sata[order(Converted_Cost, -is_basic_land, Name)][Maindeck ==  1]#[Converted_Cost > 0 & Converted_Cost < 7]
  side <- sata[order(Converted_Cost, Name)][Maindeck ==  0]
  #add basic lands
  #blands <- sata[is_basic_land == TRUE, .(.N, image_id = max(image_id)), by = Name][order(-N)]
  # if (nrow(blands) > 0) {
  # land_rows <- data.table(Name = blands[, Name], Converted_Cost = NA, colOrder = -1, is_basic_land = TRUE, Maindeck = 1, image_id = blands[, image_id], MID = blands[, Name])
  # }
  # add_basics <- rbind(land_rows, sorted)

  #laske jarjestyes
  sorted[, order_No := seq_len(.N), by = Converted_Cost]
  max_order_no <- sorted[, max(order_No)]
  side[, order_No := seq_len(.N) + max_order_no, by = Converted_Cost]


  kaadettu_main_ids <- dcast.data.table(sorted, formula =   order_No ~ colOrder, value.var = "image_id" )

  #tarkista eka, etta sideissa on kortteja
  if(nrow(side) > 0){
    kaadettu_side_ids <-  dcast.data.table(side, formula =   order_No ~ colOrder, value.var = "image_id" )
    kaadettu_side_nimi <-  dcast.data.table(side, formula =   order_No ~ colOrder, value.var = "Name" )
  }
  #append side ja maini ja sidevälirivi
  siderivi <- data.table("-1" = "Side")
  kaadettu_ids <- rbind(kaadettu_main_ids, siderivi, kaadettu_side_ids, fill = TRUE)[, order_No := NULL]
  kaadettu_ids[, order_No := seq_len(.N)]



  kaadettu_main_nimi <- dcast.data.table(sorted, formula =   order_No ~ colOrder, value.var = "Name" )

  #append side ja maini
  kaadettu_nimi <- rbind(kaadettu_main_nimi, kaadettu_side_nimi, fill = TRUE)[, order_No := NULL]
  kaadettu_nimi[, order_No := seq_len(.N)]
  reslist <- NULL
  reslist$nimi <- kaadettu_nimi
  reslist$id <- kaadettu_ids
  reslist$maxcc <- sata[!is.na(Converted_Cost), max(Converted_Cost)]


  reslist$last_main_row <- max_order_no
  print(reslist)
  return(reslist)
}
