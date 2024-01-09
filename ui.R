source("global.R") 



dashboardPage(skin = "black",                                                             
  dashboardHeader(title = "Lichess App"),
  dashboardSidebar(
    textInput("usuario","Cual es tu usuario?", "guillebarracas"),
    actionButton("update","buscar"),
    withSpinner(textOutput("text")),#esto agrega el spinner de carga
    sidebarMenu(
      menuItem("Tablero", tabName = "dashboard"),
      menuItem("Partidas", tabName = "rawdata")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          valueBoxOutput("fechaini", width = 3),
          valueBoxOutput("eloactual", width = 3),
          valueBoxOutput("winrate", width = 3)),
        fluidRow(
          valueBoxOutput("winraten", width = 3),
          valueBoxOutput("winrateb", width = 3),
          valueBoxOutput("elopromedio", width = 3)),
        fluidRow(
          box(plotlyOutput("plot1"), width = 4),
          box(plotlyOutput("plot2"), width = 4),
          box(plotlyOutput("plot3"), width = 4)),
        fluidRow(
          box(plotlyOutput("plot4"), width = 12))),
     tabItem(tabName = "rawdata",
          DTOutput("table"))))
    )