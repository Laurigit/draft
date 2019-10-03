#tab_load_draft
tabItem(tabName = "tab_load_draft",
      fluidPage(
       fluidRow(column(6,
              actionButton("reload",
                           label = HTML('<h1> Load cards</h1>'),
                           width = '100%')),
              column(6,
                     actionButton("draft_cards",
                                  width = '100%',
                                  label = HTML('<h1>Draft cards</h1>')))),
       uiOutput("draftit")
       # ,
       # uiOutput("draftitSideBar")
      )

)
