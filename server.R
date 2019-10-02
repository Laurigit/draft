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
# Define UI for application that draws a histogram

#install.packages("clipr")




# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$mytable = renderDataTable({
    #   dataSet <- read_clip_tbl(x=read_clip(),
    #                            row.names = NULL,
    #                            sep = "\t",
    #                            stringsAsFactors = FALSE)
    # })




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

  observe({
    #input <- NULL
   # input <- c("eka", "toka", "img1", "img3332")
    #imagelist <- names(input)
    imagelist <- ADM_VISUALIZE_CARDS[Pakka_form_ID == 70, image_id]
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
    isolate(values$lastAccepted <-  values$lastUpdated)

  })

  output$show_last <- renderPrint({

ADM_VISUALIZE_CARDS[image_id == values$lastUpdated  , Name]

  #  str(input[[values$lastUpdated]])

  })


}


