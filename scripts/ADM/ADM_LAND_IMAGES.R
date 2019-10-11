#ADM_LAND_IMAGES

#forest 289327
#swamp 473220
#plains 473212
#mountain 221305
#island 386333
#wastes 407693

landitaulu <- data.table(Name = c("Forest","Swamp","Plains",                                  "Mountain",
                                  "Island",
                                  "Wastes")
                         ,  MID = c(289327,
                                                                                                473220,
                                                                                                473212,
                                                                                                221305,
                                                                                                386333,
                                                                                                407693),avain = "1")

#get land images
lapply(landitaulu[,MID], function(x) {
  getCardImg_full(x)
})

counttitaulu <- data.table(avain = "1", Count = seq(1:40))
joinaa <- landitaulu[counttitaulu, on = "avain", allow.cartesian = TRUE][, avain := NULL]
joinaa[, image_file := paste0("bl_",Name, Count)]

#check if there are lands already
filut <- data.table(dir("./www"))
filut[, alku := word(string = V1, start = 1, end = 1,  sep = fixed("_"))]
landit <- filut[alku == "bl", .N]
if (landit != 240) {
warning("LUODAAN LÄNDEJÄ. EI PITÄS AJAA KOVIN USEIN")
  for(landloop in 1:nrow(joinaa)) {

  land_image_id <- joinaa[landloop, image_file]
  land_count <- joinaa[landloop, Count]
  land_name <- joinaa[landloop, Name]
  peruslandi <- image_read(paste0("./www/",joinaa[landloop, MID], "_card.jpg"))
  new_land <- image_annotate(peruslandi, land_count, gravity = "north", size = 60, color = "black")
  final_land <- image_annotate(new_land, land_count, gravity = "northeast", size = 14, color = "black", location ="+10+8")
  image_write(final_land, paste0("./www/", land_image_id, "_card.jpg"), format = "jpg")
}
}
ADM_LAND_IMAGES <- joinaa[, .(Name, Count, image_file = paste0(image_file, "_card.jpg"), MID)]
