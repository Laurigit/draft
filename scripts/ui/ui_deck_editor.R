#ui_deck_editor

tabItem(tabName = "tab_deck_editor",
        fluidRow(uiOutput("filters")),

    fluidRow(column(width = 11, uiOutput("deck_editor_select_deck")),
             column(width = 1, actionButton("update_deck_filter", label = "Update")))
    ,
    textOutput("card_dragular"),
fluidRow(id = "drag_Page2", uiOutput("decklist"))

#dragula(c("elementsOutput"), id = "dragula_dl"),
#dragulaOutput("dragOut")
)
