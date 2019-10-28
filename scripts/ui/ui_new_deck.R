
#ui_new_deck
tabItem(tabName = "tab_new_deck",
fluidPage(
  fluidRow(
textInput("pakka_nimi", "Deck name"),
actionButton("add_deck", "Add deck")
),

uiOutput("dd_images"),

#dragUI(id = "kuva_Id", tags$img(src = "www.rstudio.com", width = "100px", height = "100px")),
fluidRow(
column(2, dropUI("kohde", col_n = 1)),
       column(2, dropUI("kohde2", col_n = 1)),
              column(2, dropUI("kohde3", col_n = 1)),
                     column(2, dropUI("kohde4", col_n = 1)),
                            column(2, dropUI("kohde5", col_n = 1))
  )
)

)
