# install.packages("ggplot2")
#install.packages("leaflet")
#install.packages("shiny")
#install.packages("ggpp")

install.packages('tensorflow')
library(tensorflow)
install_tensorflow()
install.packages('keras')
library(keras)
install_keras()

library(ggplot2)
library(leaflet)
library(ggpp)
library(shiny)

###  moje zainstalowanie kerasa i tenserflow 
devtools::install_github("rstudio/keras")
library(keras)
install.packages("tensorflow")
library(tensorflow)
install_tensorflow() #tutaj bedzie reset R 
library(tensorflow)
tf$constant("Hellow Tensorflow")  #TEST tutaj powienieneś dostać " tf.Tensor(b'Hellow Tensorflow', shape=(), dtype=string)'
library(keras)
install_keras()
library(datasets)
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

#Calculating the summary statistics
#summ <- df$X1 %>% 
 # group_by(variable) %>% 
  #summarize(min = min(value), max = max(value), 
   #         mean = mean(value), q1= quantile(value, probs = 0.25), 
    #        median = median(value), q3= quantile(value, probs = 0.75))


# Visualizing
s1 = summary(df$X1)
#s1<- data.frame(unclass(summary(df$X1)), check.names = FALSE, stringsAsFactors = FALSE)

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
  
  
  
  ## BUDOWA SIECI 

# Loading data  
mnist<-dataset_mnist();
mnist$train$x <- mnist$train$x/255
mnist$test$x <- mnist$test$x/255

# Building model
model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(28, 28)) %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  layer_dense(10, activation = "softmax")

summary(model)

# Compiling model (choosing optimizer)
model %>% 
  compile(
    loss = "sparse_categorical_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

# Fitting (training) model
model %>% 
  fit(
    x = mnist$train$x, y = mnist$train$y,
    epochs = 5,
    validation_split = 0.3,
    verbose = 2
  )

# Prediction
predictions <- predict(model, mnist$test$x)
head(predictions, 1)

# Performance
model %>% 
  evaluate(mnist$test$x, mnist$test$y, verbose = 0)


#IRIS 
# iris[,5] <- as.numeric(iris[,5]) -1
iris <- df
iris[,279] <- as.numeric(iris[,279]) -1

# Turn `iris` into a matrix
iris <- as.matrix(iris)

# Set iris `dimnames` to `NULL`
dimnames(iris) <- NULL

# Determine sample size
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.70, 0.30)) #createDataPartition caret

# Split the `iris` data
iris.training <- iris[ind==1, 1:278]
iris.test <- iris[ind==2, 1:278]

# Split the class attribute
iris.trainingtarget <- iris[ind==1, 279]
iris.testtarget <- iris[ind==2, 279]

# One hot encode training target values
iris.trainLabels <- to_categorical(iris.trainingtarget)

# One hot encode test target values
iris.testLabels <- to_categorical(iris.testtarget)

# Print out the iris.testLabels to double check the result
print(iris.testLabels)

# Initialize a sequential model
model <- keras_model_sequential() 

# Add layers to the model
model %>% 
  layer_dense(units = 66, activation = 'sigmoid', input_shape = c(278)) %>% 
  layer_dropout(0.001) %>%
#  layer_dense(units = 33, activation = 'relu') %>%
#  layer_dropout(0.01) %>%
  layer_dense(units = 16, activation = 'softmax')

# Print a summary of a model
summary(model)

# Compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

# Store the fitting history in `history` 
history <- model %>% fit(
  iris.training, 
  iris.trainLabels, 
  epochs = 500,
  batch_size = 5, 
  validation_split = 0.2
)

# Plot the history
plot(history)

# Plot the model loss of the training data
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")

# Plot the model loss of the test data
lines(history$metrics$val_loss, col="green")

# Confusion matrix
table(iris.testtarget, classes)
# Add legend
legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

# Plot the accuracy of the training data 
plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l")

# Plot the accuracy of the validation data
lines(history$metrics$val_acc, col="green")

# Add Legend
legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

# Predict the classes for the test data
classes <- model %>% predict(iris.test, batch_size = 128) 

# Predictions representation
head(classes, 10)

# Evaluate on test data and labels
score <- model %>% evaluate(iris.test, iris.testLabels, batch_size = 128)

# Print the score
print(score)

