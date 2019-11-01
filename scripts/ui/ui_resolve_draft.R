#ui_resolve_draft
tabItem(tabName = "tab_resolve_draft",
        fluidPage(
            fluidRow(
              column(3, radioButtons(label = "NULL", inputId = "radio_first_pick", choices = c("Lauri", "Martti", "Not selected"))),
              column(3, actionButton(inputId = "random_first_pick", label = "Randomize first pick"))),
            fluidRow(
              uiOutput("show_resolvable_drafts")
            ),
            uiOutput("pickorders")



        )
)
