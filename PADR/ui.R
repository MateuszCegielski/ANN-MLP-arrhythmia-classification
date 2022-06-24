#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cerulean"),
  navbarPage(
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
                         max = 50,
                         value = 30
             ),
             actionButton("button", "Potwierdz"
             ),
             
             mainPanel(
               textOutput(outputId = "text")
             )
             
             
             
             
             ),#navbar 2         
    
             
             
)
)
)