#ui_pick_cards
tabItem(tabName = "tab_pick_cards",
fluidPage(
  fluidRow(
  column(4, uiOutput("select_booster")),
  column(4, actionButton("save_picks", label = "Save ordered picks")),
  column(4, uiOutput(outputId = "myLegalColors"))


  ),
  fluidRow(
    column(10, uiOutput("picK_order")),
    column(2,   uiOutput("card_order_text"))
    )

),
 dragula(c("picK_order"), id = "dragula")
)
