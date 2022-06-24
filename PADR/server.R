#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)

library(ggplot2)
library(leaflet)
library(ggpp)
library(keras)
library(tensorflow)
library(datasets)





# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Reading data
  path <- "C:/Users/admin/OneDrive/Dokumenty/STUDIA/sem6/PADR/projekty/Cegielski_Ryzhankow/Arrhythmia_MLR/arrhythmia.data"
  df <- read.table(path)
  
  # Splitting data
  df <- data.frame(do.call("rbind", strsplit(as.character(df$V1), ",", fixed = TRUE)))
  
  # Working with missing data
  # Replacing ? by NA
  df[df == "?"] <- NA
  
  # Deleting a row with too many NA's
  df <- subset(df, select=-X14)
  
  # Converting characters into numeric and replacing NA's with the mean of it's column
  for(i in 1:ncol(df)) {
    df[ , i] <- as.numeric(df[ , i])
    df[ , i][is.na(df[ , i])] <- mean(df[ , i], na.rm = TRUE)
  }

    output$wykresPlot <- renderPlot({
      
      
      if(input$wykres == 1){return(p1)}
      if(input$wykres == 2){return(p2)}
      if(input$wykres == 3){return(p3)}
      if(input$wykres == 4){return(p4)}
      if(input$wykres == 5){return(p5)}
      if(input$wykres == 6){return(p6)}
      if(input$wykres == 7){return(p7)}
      if(input$wykres == 8){return(p8)}
      if(input$wykres == 9){return(p9)}
      if(input$wykres == 10){return(p10)}
      if(input$wykres == 11){return(p11)}
      if(input$wykres == 12){return(p12)}
      if(input$wykres == 13){return(p13)}
      if(input$wykres == 14){return(p15)}
      if(input$wykres == 15){return(p21)}
      if(input$wykres == 16){return(p280)}
      
    })
    
    abc<-eventReactive(input$button, {
      runif(input$numb_h_n)
    })
    
    output$text <- renderText({
      input$numb_h_n
      
    })
    
    

})
