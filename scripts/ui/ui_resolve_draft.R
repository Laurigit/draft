#ui_resolve_draft

tabItem(tabName = "tab_resolve_draft",
        tags$style(".topimg {
                            margin-left:-30px;
                            margin-right:-30px;
                            margin-top:-15px;
                          }"),
        fluidPage(

            fluidRow(
              column(3, radioButtons(label = "First pick", inputId = "radio_first_pick", choices = c("Lauri", "Martti", "Not selected"))),
              column(3, actionButton(inputId = "random_first_pick", label = "Randomize first pick")),
              column(3, actionButton(inputId = "unlock_later_first_pick", label = "Unlock first pick")),
              column(3, actionButton(inputId = "accept_and_save", label = "Accept and save"))),


            fluidRow(
              uiOutput("show_resolvable_drafts")
            ),
            fluidRow(
            uiOutput("pickorders")
            )



        )
)

