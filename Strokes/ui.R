## 
##Claudia Donahue
# Final Project Shiny App
# ST558, Summer 2022
#

library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(rpart)
library(randomForest)

################# Read in and clean the data #################
data <- readr::read_csv(file = "../strokeData.csv",
                        show_col_types = FALSE)
# Make all categorical columns into factors
data$stroke <- factor(data$stroke)
data$gender <- factor(data$gender)
data$ever_married <- factor(data$ever_married)
data$work_type <- factor(data$work_type)
data$Residence_type <- factor(data$Residence_type)
data$smoking_status <- factor(data$smoking_status)
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
df$hypertension <- factor(df$hypertension) 
df$heart_disease <- factor(df$heart_disease) 
df$stroke <- factor(df$stroke) 

################# Define UI #################
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
                          tags$li("Data Exploration: summarize and plot user-selected data"),
                          tags$li("Modeling: fit and compare multiple supervised learning models, and make predictions on user-designed observation"),
                          tags$li("Data: browse through the data set, and save a customized file"))
                      )
                      ),
################# Data Exploration Tab #################
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
################# Modeling Tab #################
             navbarMenu("Modeling",
                        
  # Subtab for Info
                        tabPanel("Info",
                                 mainPanel(
                                   h3("Modeling Information"),
                                   h4("We will fit three different types of classification model, each one a supervised learning technique using known-outcome data to make predictions on new data. In this case, the models will predict a binary outcome: whether or not an observation in the data set had a stroke."),
                                   br(),
                                   h4("The types of machine learning algorithm we will use are: "),
                                   tags$ul(
                                     tags$li("Logistic Regression: an easily interpreted generalized linear model used to predict the probability of an outcome being a binary class based on one or more explanatory features. Unlike linear regression, this technique uses the logit link function as the response, instead of just Y itself."),
                                     withMathJax(
                                       helpText("Logistic Function: $$P = \\frac{1}{1 + e^{-{\\beta_0 + \\beta_1x}}}$$")
                                     ),
                                     p("Logistic regression does not require high computational power, and can easily be updated to reflect new data. Drawbacks include high sensitivity to outliers,  high potential for overfitting data in cases where the number of features exceeds the number of observations, and that logistic regression cannot solve non-linear problems. "),
                                     br(),
                                     tags$li("Classification Tree: this model splits up predictor space into regions, making different predictions for each region, often using the most prevalent class in the region as the predicted outcome. This technique produces easily interpretable results, and missing values/outliers do not have as significant impact on the model. Drawbacks include instability--a small change in data can mean a major change in the tree--and a potential increase in required computational power, compared to logistic regression."),
                                     br(),
                                     tags$li("Random Forest Classification: averages across many fitted classification trees, decreasing the variance over an individual tree's fit. Random forest uses a random subset of predictors for each tree fit. The randomness helps avoid overfitting, and the model does not require much tuning and experimentation to find the best parameters. Another benefit is the ability to calculate feature importance. Drawbacks are that random forest models take a lot of computational power to train, and are not easily interpretable, like individual decision trees can be."))
                                 )
                        ),
  ################# Subtab for Fitting #################
                        tabPanel("Fitting",
                                 h3("Model Fitting"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Select the proportion of data to keep in the training set, which we will use to fit the models. The remainder of the data will be held in the testing set. "),
                                     sliderInput("splitPct",
                                                 label = "Proportion of Data in Training Set",
                                                 min = 0.6,
                                                 max = 0.9,
                                                 value = 0.8,
                                                 step = 0.05),
                                     checkboxGroupInput("fitVars",
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
                                                             "Smoking Status" = "smoking_status")
                                                 ),
                                     br(),
                                     numericInput("mtry", 
                                                  "'mtry': Number of Variables to Randomly Sample at Each Tree Split",
                                                  value = 3,
                                                  min = 2,
                                                  max = 7,
                                                  step = 1
                                                  ),
                                     actionButton("go", "Fit My Models!")
                                   ),
                                   mainPanel(
                                     # Print out RMSE
                                     h3("Training Data Fit"),
                                     h4("Accuracy by Model: "),
                                     DTOutput("accuracy"),
                                     
                                     # Print summary of lr
                                     h4("Logistic Regression Model"),
                                     verbatimTextOutput("lrSummary"),
                                     br(),
                                     
                                     # Print summary of classTree 
                                     h4("Classification Tree Model"),
                                     verbatimTextOutput("ctreeSummary"),
                                     br(),
                                     
                                     # Print summary of rf - feature importance
                                     h4("Random Forest Model"),
                                     verbatimTextOutput("rfSummary"),
                                     verbatimTextOutput("featureImport"),
                                     br(),
                                     # Compare models on test set, report fit statistic
                                     h3("Testing Data Comparison"),
                                     h4("Confusion Matrices by Model: "),
                                     p("Logistic Regression: "),
                                     verbatimTextOutput("lrConfusionMatrix"),
                                     p("Classification Tree: "),
                                     verbatimTextOutput("ctConfusionMatrix"),
                                     p("Random Forest: "),
                                     verbatimTextOutput("rfConfusionMatrix")
                                   ))),
                                
  ############## Subtab for Prediction ###############
                        tabPanel("Prediction",
                                 h3("Model Predictions"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("modelForPred", 
                                                  label = "Choose a model to make predictions:",
                                                  choices = c("Logistic Regression" = "lr",
                                                              "Classification Tree" = "ct",
                                                              "Random Forest" = "rf")),
                                   selectInput("gender", "Gender",
                                               choices = levels(data$gender)),
                                   numericInput("age", "Age",
                                               value = 38, min = 0, max = 125,
                                               step = 1),
                                   selectInput("hypertension", "Hypertension",
                                               choices = c("Yes" = 1,
                                                           "No" = 0)),
                                   selectInput("heart_disease", "Heart Disease",
                                               choices = c("Yes" = 1,
                                                           "No" = 0)),
                                   selectInput("ever_married", "Ever Married",
                                               choices = levels(data$ever_married)),
                                   selectInput("work_type", "Work Type",
                                               choices = levels(data$work_type)),
                                   selectInput("Residence_type", "Type of Residence",
                                               choices = levels(data$Residence_type)),
                                   numericInput("avg_glucose_level", "Average Glucose Level",
                                                value = 95, min = 50, max = 275,
                                                step = 5),
                                   numericInput("bmi", "Body Mass Index",
                                                value = 28, min = 7, max = 100,
                                                step = 1),
                                   selectInput("smoking_status", "Smoking Status",
                                               choices = levels(data$smoking_status)),
                                   actionButton("goPredict", "Make Prediction")
                                   ), # end sidebarpanel

                                   
                                   mainPanel(
                                     h3("Predicted Outcome"),
                                     verbatimTextOutput("prediction")
                                     
                                     ) # end mainPanel
                                 ) # end sidebarlayout
                        ), # end tabpanel                                   
                                   ),
################# Data Tab #################
             tabPanel("Data",
                      titlePanel("Data Set"),
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("dataToSave", 
                                             "Variables to Include: ",
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
                                             selected = c("gender", "age", "hypertension")),
                          actionButton("goSave", "Save File")
                        ),
                        mainPanel(
                          DTOutput("dtable")
                        )
                      ))
  
    
)))
