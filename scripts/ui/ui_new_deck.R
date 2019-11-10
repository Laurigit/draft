required_data("ADM_LAND_IMAGES")
#ui_new_deck
tabItem(tabName = "tab_new_deck",
fluidPage(
  fluidRow(
    column(3,  uiOutput("sideWarning")),
    column(3,  textInput("pakka_nimi", "Deck name")),
    column(3,  actionButton("add_deck", "Add deck"))),
  fluidRow(
    column(3, radioButtons("basic_land_new_deck", "Basic land", choiceNames = ADM_LAND_IMAGES[, .N, by = .(MID, Name)][, Name],
                           choiceValues  = ADM_LAND_IMAGES[, .N, by = .(MID, Name)][, MID],
                           inline = TRUE)),
    column(3,numericInput(inputId = "count_of_lands", value = 1, label = "How many lands") ),
    column(3, actionButton(inputId = "add_basic_land_new_deck", label =  "Set land count"))
  ),

fluidRow(
  column(3, textAreaInput("new_deck_text_area", label = "Paste Name and MID")),
  column(3, radioButtons(inputId = "load_to_main_or_side", label = "Side or Main", choices = c("Main", "Side"))),
  column(3, actionButton("load_new_deck_MIDS", label = "Load cards"))
),
fluidRow(uiOutput("show_new_deck"))
)


)
