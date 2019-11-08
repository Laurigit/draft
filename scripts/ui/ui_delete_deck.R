required_data("STG_DECKS_DIM")
valinnat <- STG_DECKS_DIM[, Nimi]

#ui_new_deck
tabItem(tabName = "tab_delete_deck",


selectInput("delete_deck", label = "Retire deck", choices = valinnat),
actionButton("confirm_retire", label = "Confirm retirement")

)
