library(shiny)
library(data.table)
library(stringr)
library(shinydashboard)
library(httr)
library(jsonlite)
library(RMySQL)
library(shinyjs)
library(readtext)
library(clipr)
library(qdapRegex)
library(magick)
library(dragulaR)
library(lubridate)
options(shiny.trace=FALSE)
options(shiny.fullstacktrace = FALSE)


sourcelist <- data.table(polku = c(dir("./scripts/", recursive = TRUE)))
sourcelist[, rivi := seq_len(.N)]
sourcelist[, kansio := strsplit(polku, split = "/")[[1]][1], by = rivi]
sourcelist <- sourcelist[!grep("load_scripts.R", polku)]
sourcelist[, kansio := ifelse(str_sub(kansio, -2, -1) == ".R", "root", kansio)]

input_kansio_list <- c("utility",
                       "solution_functions",
                       "UID")
for(input_kansio in input_kansio_list) {
  dir_list <- sourcelist[kansio == input_kansio, polku]
  for(filename in dir_list) {
    result = tryCatch({
      print(paste0("sourcing ", filename))
      source(paste0("./scripts/", filename), local = TRUE)
      print(paste0("sourced ", filename))
    }, error = function(e) {
      print(paste0("error in loading file: ", filename))
    })
  }
}
#con <- connDB(con)
#rm(con)
#ÄLÄ POISAT
rm(STAT_PFI)
#ÄLÄ POISTA LOPPU
con <- connDB(con, "betmtg2")

dbSendQuery(con, 'SET NAMES utf8')
dbQ("SHOW TABLES", con)
