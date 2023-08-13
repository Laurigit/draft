# makeElement <- function(data, name)
# {
#   div(style = "border-width:2px;border-style:solid;",
#       drag = name,
#       div(class = "active title", name),
#       div(class = "active content", p(sprintf("Class: %s", class(data[[name]])))))
# }
#ui_setup_draft
tabItem(tabName = "tab_setup_draft",
        fluidPage(
         # verbatimTextOutput("order"),
            fluidRow(column(4, textAreaInput(inputId = "loadSetupDraft_area", label = "Load full booster", height = "400px", width = "400px"))),
                    fluidRow(
                      column(4, actionButton(inputId = "load_setup_draft", "Load cards")),
                     column(4, actionButton(inputId = "save_to_be_drafted", "Save as draft cards")),
                     column(4, textOutput(outputId = "card_count_output"))
                      ),


           # uiOutput("dnd_draft"),
            fluidRow(
              uiOutput("dnd_draft")
            )
          #
        )
       # fluidRow(

                # div(id = "Available", style = "min-height: 100px;min-width: 600px; max-height: 200px;",
                    # lapply(colnames(mtcars), makeElement, data = mtcars))
                  #  tags$img(src = "262838_card.jpg", height = "200px", drag = "262838_card.jpg")
          #        uiOutput("dnd_draft")
                   # )



       # dragulaOutput("dragula")
       # dragula(c("dnd_draft", "dnd_picks"), id = "dragula")
)
