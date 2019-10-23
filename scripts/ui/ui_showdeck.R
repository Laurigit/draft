#tab_leaderboard
required_data("ADM_VISUALIZE_CARDS")
tabItem(tabName = "tab_showdeck",
        fluidPage(
          fluidRow( uiOutput("deck_selector"))
         ,
         fluidRow(
           column(width = 2,
                  actionButton(inputId = "save_changes_button", "Save changes")),
           column(width = 4,
           radioButtons("main_side", "main or side", choices = list("Main", "Side", "Both", "Neither"),
                        inline = TRUE)),
           column(width = 4,

                  radioButtons("basic_land", "Basic land", choiceNames = ADM_LAND_IMAGES[, .N, by = .(MID, Name)][, Name],
                               choiceValues  = ADM_LAND_IMAGES[, .N, by = .(MID, Name)][, MID],
                              inline = TRUE)),
           # radioButtons("basic land", "Basic land", choices = list("Plains",
           #                                                         "Mountain",
           #                                                         "Swamp",
           #                                                         "Island",
           #                                                         "Forest",
           #                                                         "Wastes"),
           #             inline = TRUE)),
           column(width = 1, actionButton(inputId = "add_basic_land", label =  "Add land")),
           column(width = 1, actionButton(inputId = "remove_basic_land", label = "Remove Land"))
         ),

        #imageOutput("img2985"),
       # uiOutput("kortti"),
        fluidRow(    uiOutput("boxes"))

     #  box(id = paste0("box ", "boxno"),  paste0("box ", "boxno"))

  )
)
