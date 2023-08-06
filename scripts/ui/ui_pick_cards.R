#ui_pick_cards


tabItem(tabName = "tab_pick_cards",
fluidPage(
  fluidRow(
  column(3, uiOutput("select_booster")),
  column(3, actionButton("save_picks", label = "Save ordered picks")),
  column(3, uiOutput(outputId = "myLegalColors"))


  ),
  fluidRow(
    column(2, selectInput("allow_delete_booster", "Delete booster", choices = c("Yes", "No"), selected = "No")),
    column(2,  actionButton(inputId = "button_delete_booster", label = "Delete this booster")),
    column(2, radioButtons(label = "First pick", inputId = "radio_first_pick_correct", selected = "Not selected", choices = c("Lauri", "Martti", "Not selected"))),
    column(2, actionButton(inputId = "random_first_pick_correct", label = "Randomize first pick")),
    column(2, actionButton(inputId = "lock_first_pick", label = "Lock first pick")),
    column(2, actionButton(inputId = "unlock_first_pick", "Unlock first pick"))
  ),
  fluidRow(
    column(10, uiOutput("picK_order")),
    column(2,   uiOutput("card_order_text"))
    )

),
 dragula(c("picK_order"), id = "dragula"),
tags$style(
  HTML("
           #picK_order {

        overscroll-behavior-y: contain;
     touch-action: none;



      }
           ")
)
)
