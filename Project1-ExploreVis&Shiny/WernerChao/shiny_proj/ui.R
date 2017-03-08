library(shiny)

shinyUI(pageWithSidebar(
  headerPanel('NBA Shiny App Project'),
  sidebarPanel(
    h1('Teams')
  ),
  mainPanel(
    plotOutput("plot")
  )
))


