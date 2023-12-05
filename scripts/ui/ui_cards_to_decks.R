#ui_cards_to_decks

tabItem(tabName = "tab_cards_to_decks",
        # fluidPage(
        #   fluidRow(
        #
        #     column(width = 3, h2("Drafted cards"), uiOutput("Drafted_cards_column", style = "min-height:200px;background-color:grey;")),
        #     lapply(mydecks, function(deck_name) {
        #       column(width = 3, h2("deck_name"), uiOutput(deck_name, style = "min-height:200px;background-color:grey;"))
        #     })
        #
        #     # column(width = 3, h2("Output"), uiOutput("D2", style = "min-height:200px;background-color:grey;")),
        #     # column(width = 3, h2("kolme"), uiOutput("D3", style = "min-height:200px;background-color:grey;")),
        #     # column(width = 3, h2("nelja"), uiOutput("D4", style = "min-height:200px;background-color:grey;")),
        #
        #   ),
        #
        #   dragula(c("Drafted_cards_column", mydecks), id = "drag_cards_to_deck")
        # )
        uiOutput("deck_info_top"),
        uiOutput("deck_column"),
        uiOutput("deck_info_bot")

)
