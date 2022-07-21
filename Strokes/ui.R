##Solution to first app homework  
##Claudia Donahue
# Final Project Shiny App
# ST558, Summer 2022
#

library(shiny)
library(caret)
library(tidyverse)
library(DT)

################# Read in and clean the data #################
data <- readr::read_csv(file = "../strokeData.csv",
                        show_col_types = FALSE)
# Make gender/ married columns into factors
data$gender <- factor(data$gender)
data$ever_married <- factor(data$ever_married)
# drop id column
data <- dplyr::select(data, -id)
# change bmi to a number, and "N/A" to NA
data$bmi <- as.numeric(data$bmi)

# Make some dummy variables
# gender, work_type, Residence_type, smoking_status
dummies <- dummyVars(age ~ ., data = data)
addDummy <- data.frame(predict(dummies, newdata = data))
# keep the columns I want
data <- data.frame(cbind(data[,c(1:5, 8:9,11)], addDummy[,c(8:14, 17:20)]))
# Reorder columns so stroke is last 
columnOrder <- c(1:7, 9:19, 8)
data <- data[, columnOrder]

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predicting a Stroke in Medical Patients"),
  
  # Pages
  navbarPage("Menu:",
             tabPanel("About",
                      mainPanel(
                        h4("The purpose of this app is to explore stroke data from Kaggle and to try to identify a model to predict whether a patient will get a stroke based on available health data."),
                        br(),
                        img(src = "strokePic.jpg", width = "100px"),
                        h4(a(href = "https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset", "The data"), "from Kaggle contains observations from a confidential source. "),
                        br(),
                        h4("The navigation bar contains links to the following pages: "),
                        tags$ul(
                          tags$li("Data Exploration: summarize and plot selected data"),
                          tags$li("Modeling: fit multiple supervised learning models, and make predictions using the selected model"),
                          tags$li("Data: browse through the data set, and save a file"))
                      )
                      
                      ),
             
             tabPanel("Data Exploration",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("var", label = "Variables to Summarize", 
                                      choices = c("Age" = "age",
                                                  "Glucose Level" = "avg_glucose_level",
                                                  "Body Mass Index (BMI)" = "bmi"),
                                      selected = "Age")
                        ),
                        mainPanel(
                          dataTableOutput("dataTable")
                        )
                      )),
             navbarMenu("Modeling",
                        tabPanel("Info"),
                        tabPanel("Fitting"),
                        tabPanel("Prediction")),
             tabPanel("Data")
  )
    
  )
)
