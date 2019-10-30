makeElement <- function(data, name)
{
  div(style = "border-width:2px;border-style:solid;",
      drag = name,
      div(class = "active title", name),
      div(class = "active content", p(sprintf("Class: %s", class(data[[name]])))))
}
#ui_setup_draft
tabItem(tabName = "tab_setup_draft",
        fluidPage(
          verbatimTextOutput("order"),
            fluidRow(column(6, textAreaInput(inputId = "loadSetupDraft_area", label = "Load full booster")),
                      column(6, actionButton(inputId = "load_setup_draft", "Load cards"))),
           # uiOutput("dnd_draft"),
            fluidRow(
              div(id = "draft",column(width = 3, h2("Output"), uiOutput("dnd_picks", style = "min-height:200px;background-color:grey;")))
            )
          #
        ),
        fluidRow(

                 div(id = "Available", style = "min-height: 100px;min-width: 600px; max-height: 200px;",
                    # lapply(colnames(mtcars), makeElement, data = mtcars))
                  #  tags$img(src = "262838_card.jpg", height = "200px", drag = "262838_card.jpg")
                  uiOutput("dnd_draft")
                    )
          ),
        fluidRow(

                 div(id = "Model", style = "min-height: 600px;")

        ),
       # dragulaOutput("dragula")
        dragula(c("dnd_draft", "dnd_picks"), id = "dragula")
)
