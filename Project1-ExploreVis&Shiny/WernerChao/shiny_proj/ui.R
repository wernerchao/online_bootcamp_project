library(shiny)

shinyUI(pageWithSidebar(
  headerPanel(h1('NBA Shiny App Project', align='center')),
  sidebarPanel(
    h3('Choose Distance Types'), 
    selectInput('distance',
                label='Choose the distance to see associated shot %',
                choices= c('< 8ft', '8-16ft', '16-24ft', '> 24ft')
                )
  ),
  mainPanel(
    plotOutput("plot")
  )
))


