#ui_cards_to_decks

tabItem(tabName = "tab_cards_to_decks",
        fluidPage(
          fluidRow(
            column(width = 3, h2("Drafted cards"), uiOutput("Drafted_cards_column", style = "min-height:200px;background-color:grey;")),
            column(width = 3, h2("Output"), uiOutput("D2", style = "min-height:200px;background-color:grey;")),
            column(width = 3, h2("kolme"), uiOutput("D3", style = "min-height:200px;background-color:grey;")),
            column(width = 3, h2("nelja"), uiOutput("D4", style = "min-height:200px;background-color:grey;")),

          ),

          dragula(c("Drafted_cards_column", "D2", "D3", "D4"), id = "drag_cards_to_deck")
        )
)
