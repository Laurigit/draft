#
#
# output$decklist <- renderUI({
#
# required_data("STG_CARDS_DIM")
#   sscols_cards <- STG_CARDS_DIM[, .(MID, Name)]
#   kierrosKuva <- table_to_render[1]
#   fluidPage(
#     # for (sarake in 1:table_to_render[, max(x)]) {
#     #
#     # }
#     lapply(1:table_to_render[, uniqueN(x)], function(rivi) {
#       column(width = 1,
#              offset = 0,
#                  lapply(1:table_to_render[x == rivi, uniqueN(y)], function(sarake) {
#                    nimi <- table_to_render[x == rivi & y == sarake, Name]
#                    MIDi <- sscols_cards[Name == nimi, MID]
#                    getCardImg_full(MIDi)
#                    output[[nimi]] <-  renderImage({
#
#                      # output[[image_id]] <-  renderImage({
#                      list(src = paste0("./www/", MID, "_card.jpg"),#image_nm,
#                           alt = "Image failed to render"
#                      )
#                    }, deleteFile = FALSE)
#                    imageOutput(nimi,
#                                height = "100px")
#                  })
#       )
#     })
#
# )
# })
#
#
