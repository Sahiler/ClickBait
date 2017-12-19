
library(shiny)

shinyUI(fluidPage(

  sidebarLayout(
    sidebarPanel(
      selectInput("varType","Factor:",c("Overall Score","Relation","Sentiment")),

      selectInput("src","Source1:",c("CNN","USAToday","NYPost","Daily Mail"), selected = "CNN"),

      selectInput("src2","Source2:",c("CNN","USAToday","NYPost","Daily Mail"), selected = "Daily Mail"),

      radioButtons("plotType","Plot Type:",c("Histogram" = "hist","Boxplot" = "box"))

    ),

    mainPanel(
      plotOutput("distPlot")
    )
  )
))
