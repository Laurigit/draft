
#ui_new_deck
tabItem(tabName = "tab_new_deck",
fluidPage(
  fluidRow(
textInput("pakka_nimi", "Deck name"),
actionButton("add_deck", "Add deck"),
actionButton("load_new_deck_MIDS", label = "Load cards"),
radioButtons(inputId = "load_to_main_or_side", label = "Side or Main", choices = c("Main", "Side"))
),
fluidRow(
  textAreaInput("new_deck_text_area", label = NULL)
),
fluidRow(uiOutput("show_new_deck"))
)


)
