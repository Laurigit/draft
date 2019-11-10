required_data("ADM_LAND_IMAGES")
#ui_new_deck
tabItem(tabName = "tab_new_deck",
fluidPage(
  fluidRow(
    uiOutput("sideWarning"),
textInput("pakka_nimi", "Deck name"),
actionButton("add_deck", "Add deck"),
actionButton("load_new_deck_MIDS", label = "Load cards"),
radioButtons(inputId = "load_to_main_or_side", label = "Side or Main", choices = c("Main", "Side")),

radioButtons("basic_land_new_deck", "Basic land", choiceNames = ADM_LAND_IMAGES[, .N, by = .(MID, Name)][, Name],
             choiceValues  = ADM_LAND_IMAGES[, .N, by = .(MID, Name)][, MID],
             inline = TRUE),
numericInput(inputId = "count_of_lands", value = 1, label = "How many lands"),
actionButton(inputId = "add_basic_land_new_deck", label =  "Set land count")



),
fluidRow(
  textAreaInput("new_deck_text_area", label = NULL)
),
fluidRow(uiOutput("show_new_deck"))
)


)
