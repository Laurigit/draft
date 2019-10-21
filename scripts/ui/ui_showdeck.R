#tab_leaderboard
tabItem(tabName = "tab_showdeck",
        uiOutput("deck_selector"),
        actionButton(inputId = "save_changes_button", "Save changes"),
        radioButtons("main_side", "main or side", choices = list("Main", "Side", "Both", "Neither"),
                     inline = TRUE),
        #imageOutput("img2985"),
       # uiOutput("kortti"),

       uiOutput("boxes")
     #  box(id = paste0("box ", "boxno"),  paste0("box ", "boxno"))

)
