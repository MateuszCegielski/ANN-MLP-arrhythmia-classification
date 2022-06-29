library(keras)
library(ggplot2)
library(leaflet)
library(ggpp)
library(shiny)
library(shinythemes)
library(leaflet)


################
#Working on data
################

# Reading data
path <- "Arrhythmia_MLR/arrhythmia.data"
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

# Visualizing
s1 = summary(df$X1)

colnames(s1) = c("Dane")



(p1<-ggplot(df,aes(`X1`))+
    geom_histogram(bins = 15,binwidth = 2, stat = 'count')+
    labs(x="Wiek",y="Liczba zliczeń",title = " Cecha nr 1 – wiek w latach")+
    annotate("text",x= 10,y=15,label = paste0("Średnia: ",as.character(round(mean(df$X1),2)),"\nOddchylenie std :",as.character(round(sd(df$X1),2) ))))

s2 <- summary(df$X2)
(p2<-ggplot(df,aes(x=factor(`X2`)))+
    geom_histogram(stat = 'count')+
    labs(x="Płeć",y="Liczba zliczeń",title = "Cecha nr 2 – płeć (0 = male, 1 = female")+
    annotate("text",x= 1,y=300,label = paste0("Średnia: ",as.character(round(mean(df$X2),2)),"\nOddchylenie std :",as.character(round(sd(df$X2),2) ))))




s3 <- summary(df$X3)
(p3<-ggplot(df,aes(x=factor(`X3`)))+
    geom_histogram(stat = 'count')+
    labs(x="Wzrost",y="Liczba zliczeń",title = "Cecha nr 3 – wzrost [cm]")+
    annotate("text",x= 10,y=40,label = paste0("Średnia: ",as.character(round(mean(df$X3),2)),"\nOddchylenie std :",as.character(round(sd(df$X3),2) ))))

s4 <- summary(df$X4)
(p4<-ggplot(df,aes(x=factor(`X4`)))+
    geom_histogram(stat = 'count')+
    labs(x="Waga",y="Liczba zliczeń",title = "Cecha nr 4 – waga [kg]")+
    annotate("text",x= 10,y=30,label = paste0("Średnia: ",as.character(round(mean(df$X4),2)),"\nOddchylenie std :",as.character(round(sd(df$X4),2) ))))


s5 <- summary(df$X5)
(p5<-ggplot(df,aes(x=factor(`X5`)))+
    geom_histogram(stat = 'count')+
    labs(x="Średni czas trwania QRS [msec]",y="Liczba zliczeń",title = "Cecha nr 5 – średni czas trwania QRS [msec]")+
    annotate("text",x= 10,y=30,label = paste0("Średnia: ",as.character(round(mean(df$X5),2)),"\nOddchylenie std :",as.character(round(sd(df$X5),2) ))))

s6 <- summary(df$X6)
(p6<-ggplot(df,aes(x=factor(`X6`)))+
    geom_histogram(stat = 'count')+
    labs(x="średni czas trwania między początkiem fal P i Q [msec]",y="Liczba zliczeń",title = "Cecha nr 6 – średni czas trwania między początkiem fal P i Q [msec]")+
    annotate("text",x= 15,y=20,label = paste0("Średnia: ",as.character(round(mean(df$X6),2)),"\nOddchylenie std :",as.character(round(sd(df$X6),2) ))))

s7 <- summary(df$X7)
(p7<-ggplot(df,aes(x=factor(`X7`)))+
    geom_histogram(stat = 'count')+
    labs(x="średni czas trwania między początkiem Q a przesunięciem fal T [msec]",y="Liczba zliczeń",title = "Cecha nr 7 – średni czas trwania między początkiem Q a przesunięciem fal T [msec]
")+
    annotate("text",x= 15,y=15,label = paste0("Średnia: ",as.character(round(mean(df$X7),2)),"\nOddchylenie std :",as.character(round(sd(df$X7),2) ))))

s8 <- summary(df$X8)
(p8<-ggplot(df,aes(x=factor(`X8`)))+
    geom_histogram(stat = 'count')+
    labs(x="średni czas trwania fali T [msec]",y="Liczba zliczeń",title = "Cecha nr 8 – średni czas trwania fali T [msec]")+
    annotate("text",x= 15,y=15,label = paste0("Średnia: ",as.character(round(mean(df$X8),2)),"\nOddchylenie std :",as.character(round(sd(df$X8),2) ))))

s9 <- summary(df$X9)
(p9<-ggplot(df,aes(x=factor(`X9`)))+
    geom_histogram(stat = 'count')+
    labs(x="średni czas trwania fali P [msec]",y="Liczba zliczeń",title = "Cecha nr 9 – średni czas trwania fali P [msec]")+
    annotate("text",x= 15,y=20,label = paste0("Średnia: ",as.character(round(mean(df$X9),2)),"\nOddchylenie std :",as.character(round(sd(df$X9),2) ))))

s10 <- summary(df$X10)
(p10<-ggplot(df,aes(x=factor(`X10`)))+
    geom_histogram(stat = 'count')+
    labs(x="kąt wektorowy QRS  na płaszczyźnie przedniej",y="Liczba zliczeń",title = "Cecha nr 10 – kąt wektorowy QRS  na płaszczyźnie przedniej")+
    annotate("text",x= 25,y=10,label = paste0("Średnia: ",as.character(round(mean(df$X10),2)),"\nOddchylenie std :",as.character(round(sd(df$X10),2) ))))

s11 <- summary(df$X11)
(p11<-ggplot(df,aes(x=factor(`X11`)))+
    geom_histogram(stat = 'count')+
    labs(x="kąt wektorowy T na płaszczyźnie przedniej",y="Liczba zliczeń",title = "Cecha nr 11 – kąt wektorowy T na płaszczyźnie przedniej")+
    annotate("text",x= 30,y=10,label = paste0("Średnia: ",as.character(round(mean(df$X11),2)),"\nOddchylenie std :",as.character(round(sd(df$X11),2) ))))

s12 <- summary(df$X12)
(p12<-ggplot(df,aes(x=factor(`X12`)))+
    geom_histogram(stat = 'count')+
    labs(x="kąt wektorowy P na płaszczyźnie przedniej",y="Liczba zliczeń",title = "Cecha nr 12 – kąt wektorowy P na płaszczyźnie przedniej")+
    annotate("text",x= 20,y=20,label = paste0("Średnia: ",as.character(round(mean(df$X12),2)),"\nOddchylenie std :",as.character(round(sd(df$X12),2) ))))

s13 <- summary(df$X13)
(p13<-ggplot(df,aes(x=factor(`X13`)))+
    geom_histogram(stat = 'count')+
    labs(x="kąt wektorowy QRST na płaszczyźnie przedniej",y="Liczba zliczeń",title = "Cecha nr 13 – kąt wektorowy QRST na płaszczyźnie przedniej")+
    annotate("text",x= 30,y=10,label = paste0("Średnia: ",as.character(round(mean(df$X13),2)),"\nOddchylenie std :",as.character(round(sd(df$X13),2) ))))

s15 <- summary(df$X15)
(p15<-ggplot(df,aes(x=factor(`X15`)))+
    geom_histogram(stat = 'count')+
    labs(x="liczba uderzeń serca na minutę",y="Liczba zliczeń",title = "Cecha nr 15 – liczba uderzeń serca na minutę")+
    annotate("text",x= 30,y=30,label = paste0("Średnia: ",as.character(round(mean(df$X15),2)),"\nOddchylenie std :",as.character(round(sd(df$X15),2) ))))

s21 <- summary(df$X21)
(p21<-ggplot(df,aes(x=factor(`X21`)))+
    geom_histogram(stat = 'count')+
    labs(x="liczba ugięć wewnętrznych",y="Liczba zliczeń",title = "Cecha nr 21 – liczba ugięć wewnętrznych")+
    annotate("text",x= 15,y=90,label = paste0("Średnia: ",as.character(round(mean(df$X21),2)),"\nOddchylenie std :",as.character(round(sd(df$X21),2) ))))

s280 <- summary(df$X280)
(p280<-ggplot(df,aes(x=factor(`X280`)))+
    geom_histogram(stat = 'count')+
    labs(x="Numer klasy",y="Liczba zliczeń dla klasy ",title = "Histogram klas ")+
    annotate("text",x= 15,y=90,label = paste0("Średnia: ",as.character(round(mean(df$X280),2)),"\nOddchylenie std :",as.character(round(sd(df$X280),2) ))))


# Reducing numbers of the first class examples
rows_to_delete <- c()

for(i in 452:120) {
  if (df[i, 'X280'] == 1){
    rows_to_delete <- append(rows_to_delete, i)
  } 
}

df <- df[-rows_to_delete, ]

df[,279] <- as.numeric(df[,279]) -1

# Turn `df` into a matrix
df <- as.matrix(df)

# Set df `dimnames` to `NULL`
dimnames(df) <- NULL

# Determine sample size
ind <- sample(2, nrow(df), replace=TRUE, prob=c(0.70, 0.30)) #createDataPartition caret

# Split the `df` data
df.training <- df[ind==1, 1:278]
df.test <- df[ind==2, 1:278]

# Split the class attribute
df.trainingtarget <- df[ind==1, 279]
df.testtarget <- df[ind==2, 279]

# One hot encode training target values
df.trainLabels <- to_categorical(df.trainingtarget)

# One hot encode test target values
df.testLabels <- to_categorical(df.testtarget)


########################################################################

################
#Net
################

# Użytkownik wybiera pomiędzy 'mean_squared_error', 'mean_squared_logarithmic_error'oraz 'binary_crossentropy'
loss_fuction <- 'binary_crossentropy'

# Użytkownik wybiera liczbę neuronóW
nr_units <- 10

# Initialize a sequential model
model <- keras_model_sequential() 

# Add layers to the model
model %>% 
  layer_dense(units = nr_units, activation = 'linear', input_shape = c(278)) %>% 
  layer_dense(units = 16, activation = 'softmax')

# Print a summary of a model
summary(model)

# Compile the model
model %>% compile(
  loss = loss_fuction,
  optimizer = optimizer_adam(learning_rate = 0.01),
  metrics = 'accuracy'
)

# Store the fitting history in `history` 
history <- model %>% fit(
  df.training, 
  df.trainLabels, 
  epochs = 50,
  batch_size = 5, 
  validation_split = 0.2,
  callbacks= list(
    callback_model_checkpoint("checkpoints.h5", save_best_only = TRUE)
))

# Restoring the best model
new_model <- keras::load_model_hdf5("checkpoints.h5")
summary(new_model)



################
#Results
################

# Plot the history
#plot(history)

# Plot the model loss of the training data
#plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")

# Plot the model loss of the test data
#lines(history$metrics$val_loss, col="green")

# Confusion matrix
#table(df.testtarget, classes)
# Add legend
#legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

# Plot the accuracy of the training data 
#plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l")

# Plot the accuracy of the validation data
#lines(history$metrics$val_acc, col="green")

# Add Legend
#legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

# Predict the classes for the test data
#classes <- model %>% predict(df.test, batch_size = 128) 

# Predictions representation
#head(classes, 10)

# Evaluate on test data and labels
#score <- model %>% evaluate(df.test, df.testLabels, batch_size = 128)

# Print the score
#print(score)

#####################################################################


################
#UI
################

# Define UI for application that draws a histogram
ui <- fluidPage(navbarPage(
  "My app",
  tabPanel("Wykresy",
           sidebarPanel(
             
             selectInput(
               inputId = "wykres",
               label = "Wybierz wykres: ",
               choices = list ("Cecha nr 1 - wiek w latach" = 1, 
                               "Cecha nr 2 - plec (0 = male, 1 = female" = 2,
                               "Cecha nr 3 - wzrost [cm]" = 3,
                               "Cecha nr 4 - waga [kg]" = 4,
                               "Cecha nr 5 - sredni czas trwania QRS [msec]"= 5,
                               "Cecha nr 6 - sredni czas trwania miedzy poczatkiem fal P i Q [msec]" = 6,
                               "Cecha nr 7 - sredni czas trwania miedzy poczatkiem Q a przesunieciem fal T [msec]" = 7,
                               "Cecha nr 8 - sredni czas trwania fali T [msec]" = 8,
                               "Cecha nr 9 - sredni czas trwania fali P [msec]" = 9,
                               "Cecha nr 10 - sredni wektorowy QRS  na plaszczyznie przedniej" = 10,
                               "Cecha nr 11 - kat wektorowy T na plaszczyźnie przedniej" = 11,
                               "Cecha nr 12 - kat wektorowy P na plaszczyznie przedniej" = 12,
                               "Cecha nr 13 - kat wektorowy QRST na plaszczyznie przedniej" = 13,
                               "Cecha nr 15 - liczba uderzen serca na minute" = 14,
                               "Cecha nr 21 - liczba ugiec wewnatrznych" = 15,
                               "Histogram klas" = 16
               )
             ),
           ),# sidebarPanel
           mainPanel(
             plotOutput(outputId = "wykresPlot")
             
           )#main panel 
  ), #navbar1
  tabPanel("Siec",
           tags$h2("Konfiguracja sieci"),
           sliderInput("numb_h_n",
                       "Number of hidden neurons:",
                       min = 1,
                       max = 20,
                       value = 10
           ),
           actionButton("button", "Potwierdz"
           ),
           
           mainPanel(
             textOutput(outputId = "text")
           )
  ),#navbar 2         
)
)



################
#Server

################
# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
  
  
  
}



################
#Application
################
shinyApp(ui = ui, server = server)
