#ui_deck_editor
required_data("ADM_VISUALIZE_CARDS")
tabItem(tabName = "tab_deck_editor",
        uiOutput("filters"),
    fluidRow(column(width = 2, selectInput(inputId = "choose_decklist", "Choose decklist", choices = ADM_VISUALIZE_CARDS[, unique(Pakka_form_ID)])),
             column(width = 2, actionButton("update_deck_filter", label = "Update")))
    ,
uiOutput("decklist")
)
