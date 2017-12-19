
library(shiny)
library(RCurl)
library(tm)
library(RSentiment)
library(ggplot2)
library(grid)
library(gridExtra)

data("cnn_articles", package = "RClickbait")
data("usatoday_articles", package = "RClickbait")
data("dailymail_articles", package = "RClickbait")
data("nypost_articles", package = "RClickbait")

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    if(input$src == "CNN"){
      data1 <- cnn_articles
    }
    if(input$src == "USAToday"){
      data1 <- usatoday_articles
    }
    if(input$src == "Daily Mail"){
      data1 <- dailymail_articles
    }
    if(input$src == "NYPost"){
      data1 <- nypost_articles
    }

    if(input$src2 == "CNN"){
      data2 <- cnn_articles
    }
    if(input$src2 == "USAToday"){
      data2 <- usatoday_articles
    }
    if(input$src2 == "Daily Mail"){
      data2 <- dailymail_articles
    }
    if(input$src2 == "NYPost"){
      data2 <- nypost_articles
    }

    if(input$varType == "Overall Score"){
      Source1 <- data1$score
      Source2 <- data2$score
    }
    if(input$varType == "Relation"){
      Source1 <- keyword_count(data1$title)
      Source2 <- keyword_count(data2$title)
    }
    if(input$varType == "Sentiment"){
      Source1 <- data1$sentiment
      Source2 <- data2$sentiment
    }

    if(input$plotType == "box"){
      min = min(range(Source1),range(Source2))
      max = max(range(Source1),range(Source2))
      rangegraph=range(min,max)

      p1 <- ggplot(data1,aes(x = 1 , y = Source1)) +
        geom_boxplot(col = "red") +
        scale_y_continuous(limits = rangegraph) +
        coord_flip()

      p2 <- ggplot(data2,aes(x = 1 , y = Source2)) +
        geom_boxplot(col = "blue") +
        scale_y_continuous(limits = rangegraph) +
        coord_flip()
    }

    if(input$plotType == "hist"){
      min = min(range(Source1),range(Source2))
      max = max(range(Source1),range(Source2))
      rangegraph=range(min,max)
      rangeof = max-min

      p1 <- ggplot(data1,aes(x = Source1)) +
        geom_histogram(bins = min(rangeof,30), fill = "red") +
        scale_x_continuous(limits = rangegraph)

      p2 <- ggplot(data2,aes(x = Source2)) +
        geom_histogram(bins = min(rangeof,30), fill = "blue") +
        scale_x_continuous(limits = rangegraph)
    }

    grid.arrange(p1,p2,ncol=1)
  })

})
