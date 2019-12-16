#ui_deck_editor
required_data("ADM_VISUALIZE_CARDS")
tabItem(tabName = "tab_deck_editor",
    selectInput(inputId = "choose_decklist", "Choose decklist", choices = ADM_VISUALIZE_CARDS[, unique(Pakka_form_ID)]),
uiOutput("decklist")
)
