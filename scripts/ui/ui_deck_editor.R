#ui_deck_editor
required_data("ADM_CURRENT_PAKKA")
deck_sorted <- ADM_CURRENT_PAKKA[order(Pakka_Name)]
deck_options <- deck_sorted[, Pakka_form_ID]
names(deck_options) <- deck_sorted[, Pakka_Name]

tabItem(tabName = "tab_deck_editor",
        fluidRow(uiOutput("filters")),

    fluidRow(column(width = 2, selectInput(inputId = "choose_decklist", "Choose decklist", choices = deck_options)),
             column(width = 2, actionButton("update_deck_filter", label = "Update")))
    ,
    textOutput("card_dragular"),
fluidRow(id = "drag_Page2", uiOutput("decklist")),

#dragula(c("elementsOutput"), id = "dragula_dl"),
dragulaOutput("dragOut")
)
