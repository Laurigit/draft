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
# Define UI for application that draws a histogram
ui <- fluidPage(

  dataTableOutput('mytable'),
  actionButton("paste", "Paste")
)

#install.packages("clipr")




# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mytable = renderDataTable({
      dataSet <- read_clip_tbl(x=read_clip(),
                               row.names = NULL,
                               sep = "\t",
                               stringsAsFactors = FALSE)
    })

}

# Run the application
shinyApp(ui = ui, server = server)

