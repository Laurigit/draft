#tab_leaderboard
tabItem(tabName = "tab_showdeck",
        numericInput("pif","pif", 70, 1, 1000),
        uiOutput("deck_selector"),
        radioButtons("main_side", "main or side", choices = list("Main", "Side", "Both", "Neither")),
        #imageOutput("img2985"),
       # uiOutput("kortti"),

       uiOutput("boxes"),
       actionButton(input ="sb", label = "Napp3423i")
     #  box(id = paste0("box ", "boxno"),  paste0("box ", "boxno"))

)
