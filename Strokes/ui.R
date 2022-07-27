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
# change bmi to a number
data$bmi <- as.double(data$bmi)
# replace missing bmi values with mean bmi
data$bmi[is.na(data$bmi)] <- mean(data$bmi, na.rm = TRUE)

# Make a special df for graphing
df <- data %>%
  mutate(hypertension = ifelse(hypertension == 0, "No","Yes")) %>%
  mutate(heart_disease = ifelse(heart_disease == 0, "No","Yes")) %>%
  mutate(stroke = ifelse(stroke == 0, "No","Yes"))

# Make some dummy variables
# gender, work_type, Residence_type, smoking_status
dummies <- dummyVars(age ~ ., data = data)
addDummy <- data.frame(predict(dummies, newdata = data))
# keep the columns I want
data2 <- data.frame(cbind(data[,c(1:5, 8:9,11)], addDummy[,c(8:14, 17:20)]))
# Reorder columns so stroke is last 
columnOrder <- c(1:7, 9:19, 8)
data2 <- data2[, columnOrder]

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
# Data Exploration Tab             
             tabPanel("Data Exploration",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("plotType", 
                                      label = "Type of Graph",
                                      choices = c("Bar Graph" = "bar",
                                                  "Scatter Plot" = "scatterplot"),
                                      selected = "scatterplot"),
                          # Only show this panel if the scatterplot box is selected
                          conditionalPanel(condition = "input.plotType == 'scatterplot'",
                                           selectInput("points", 
                                                       label = "Variable to Plot", 
                                                       choices = c("Type of Work" = "work_type",
                                                                   "Smoking Status" = "smoking_status",
                                                                   "Gender" = "gender"),
                                                       selected = "Type of Work")),
                          # Only show this panel if the bar box is selected
                          conditionalPanel(condition = "input.plotType == 'bar'",
                                           selectInput("bars", 
                                                       label = "Variable to Graph", 
                                                       choices = c("Hypertension" = "hypertension",
                                                                   "Heart Disease" = "heart_disease"),
                                                       selected = "Hypertension")),                          
                                           
                          selectInput("var", 
                                      label = "Variables to Summarize", 
                                      choices = c("Age" = "age",
                                                  "Glucose Level" = "avg_glucose_level",
                                                  "Body Mass Index (BMI)" = "bmi"),
                                      selected = "Age"),
                        selectInput("summType", 
                                    label = "Type of Summary",
                                    choices = c("Mean" = "mean",
                                                "Median" = "median"),
                                    selected = "Mean")
                        ),
                        mainPanel(
                          plotOutput("graph"),
                          br(),
                          DTOutput("summary")
                        )
                      )),
# Modeling Tab
             navbarMenu("Modeling",
  # Subtab for Info
                        tabPanel("Info",
                                 mainPanel(
                                   h4("We will fit three different types of classification model, a set of supervised learning techniques using known-outcome data to make predictions on new data. In this case, the models will predict whether or not an observation in the data set had a stroke."),
                                   br(),
                                   h4("The types of model we will use are: "),
                                   tags$ul(
                                     tags$li("Logistic Regression"),
                                     tags$li("Classification Tree"),
                                     tags$li("Random Forest Classification"))
                                 )
                        ),
  # Subtab for Fitting
                        tabPanel("Fitting",
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Select the proportion of data to keep in the training set, which we will use to fit the models. The remainder of the data will be held in the testing set. "),
                                     sliderInput("splitPct",
                                                 label = "Proportion of Data in Training Set",
                                                 min = 0.6,
                                                 max = 0.9,
                                                 value = 0.8,
                                                 step = 0.05),
                                     selectInput("fitVars",
                                                 label = "Variables to Use in Model Fitting",
                                                 choices = c("Hypertension" = "hypertension",
                                                             "Heart Disease" = "heart_disease",
                                                             "Gender" = "gender",
                                                             "Age" = "age",
                                                             "Ever Married" = "ever_married",
                                                             "Work Type" = "work_type",
                                                             "Residence Type" = "Residence_type",
                                                             "Glucose Level" = "avg_glucose_level",
                                                             "Body Mass Index" = "bmi",
                                                             "Smoking Status" = "smoking_status"),
                                                 selected = "age",
                                                 multiple = TRUE),
                                     actionButton("go", "Fit My Models!")
                                   ),
                                   mainPanel(
                                     
                                   ))),
                                
  # Subtab for Prediction (disappeared)
                        tabPanel("Prediction")
    ),
# Data Tab (went as a subtab under Modeling)
             tabPanel("Data")
  
    
  )
))
