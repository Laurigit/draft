#ui_deck_editor
required_data("ADM_VISUALIZE_CARDS")
tabItem(tabName = "tab_deck_editor",
        uiOutput("filters"),
    fluidRow(column(width = 2, selectInput(inputId = "choose_decklist", "Choose decklist", choices = ADM_VISUALIZE_CARDS[, unique(Pakka_form_ID)])),
             column(width = 2, actionButton("update_deck_filter", label = "Update")))
    ,
    textOutput("card_dragular"),
fluidRow(id = "drag_Page2", uiOutput("decklist")),
column(width = 3, h2("Output"), uiOutput("elementsOutput", style = "min-height:200px;background-color:grey;")),
#dragula(c("decklist", "elementsOutput"), id = "dragula_dl"),
dragulaOutput("dragOut")
)
