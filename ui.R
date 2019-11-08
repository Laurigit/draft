#library(shiny)
#library(shinydashboard)

# Define UI for application that draws a histogram

uusi_peli <- dashboardBody(

  useShinyjs(),
  useDragulajs(),
  # height: 114px;
  tags$head(
    tags$style(
      HTML("
           #logo {

    height: 60px;
     width: 146px;
     overflow: hidden;
      }
           ")
    )

  ),





  #
  #   useShinyjs(),
  #   extendShinyjs(text = jscode),
  #   extendShinyjs(text = "shinyjs.hidehead = function(parm){
  #                 $('header').css('display', parm);
  #                 }"),

  tags$head(
    tags$style(
      HTML("
           #myScrollBox{
           overflow-y: scroll;
           overflow-x: hidden;
           height:740px;
           }
           ")
    )
    ,

    tags$style(type = "text/css", "
               .irs-slider {width: 30px; height: 30px; top: 22px;}
               ")


  ),
  tabItems(
    source("./scripts/ui/ui_setup_draft.R",local = TRUE)$value,
    source("./scripts/ui/ui_load_draft.R",local = TRUE)$value,
    source("./scripts/ui/ui_new_deck.R",local = TRUE)$value,
    source("./scripts/ui/ui_showdeck.R",local = TRUE)$value,
    source("./scripts/ui/ui_pick_cards.R",local = TRUE)$value,
    source("./scripts/ui/ui_resolve_draft.R",local = TRUE)$value,
    source("./scripts/ui/ui_delete_deck.R",local = TRUE)$value
  ))



#SIDEBAR
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
              menuItem("Setup draft", tabName = "tab_setup_draft", icon = icon("gamepad")),
              menuItem("Pick cards", tabName = "tab_pick_cards", icon = icon("gamepad")),
              menuItem("Resolve draft", tabName = "tab_resolve_draft", icon = icon("gamepad")),
              menuItem("Load draft", tabName = "tab_load_draft", icon = icon("gamepad")),
              menuItem("Edit decks", tabName = "tab_showdeck", icon = icon("gamepad")),
              menuItem("New deck", tabName = "tab_new_deck", icon = icon("gamepad")),
              menuItem("New deck", tabName = "tab_delete_deck", icon = icon("gamepad")),
              actionButton("saveDraftedCards", "Save drafted cards"),
              uiOutput("show_last"),
              uiOutput("select_draft"),
              uiOutput("draftitSideBar")



  )


)

#RUNKO
dashboardPage(


  #dashboardHeader(title = paste0("run_mode = ", GLOBAL_test_mode, " ", textOutput('blow_timer')),
  #  dashboardHeader(title = textOutput('blow_timer'),
  #                 titleWidth = 450),
  dashboardHeader(title = textOutput('Username')),

  sidebar,
  uusi_peli
)





