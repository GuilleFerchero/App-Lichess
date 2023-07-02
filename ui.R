source("global.R") 



dashboardPage(                                                             
  dashboardHeader(title = "Lichess App"),
  dashboardSidebar(
    textInput("usuario","Cual es tu usuario?", "guillebarracas"),
    actionButton("update","buscar"),
    textOutput("text")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("partidas", width = 4),
      valueBoxOutput("fechaini", width = 4),
      valueBoxOutput("fechafin", width = 4)),
    fluidRow(
      box(plotlyOutput("plot1"), width = 4),
      box(plotlyOutput("plot2"), width = 8)),
    DTOutput("table")))