#ui_pick_cards
tabItem(tabName = "tab_pick_cards",
fluidPage(
  fluidRow(
  column(6, uiOutput("select_booster")),
  column(6, actionButton("save_picks", label = "Save ordered picks"))

  ),
  fluidRow(
    uiOutput("picK_order")
  )
),
 dragula(c("picK_order"), id = "dragula")
)
