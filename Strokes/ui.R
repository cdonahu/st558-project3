##Solution to first app homework  
##Claudia Donahue
# Final Project Shiny App
# ST558, Summer 2022
#

library(shiny)

# Read in data
data <- readr::read_csv(file = "../strokeData.csv",
                        show_col_types = FALSE)

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
                                      choices = c("Age",
                                                  "Glucose",
                                                  "BMI"),
                                      selected = "Age")
                        ),
                        mainPanel()
                      )),
             navbarMenu("Modeling",
                        tabPanel("Info"),
                        tabPanel("Fitting"),
                        tabPanel("Prediction")),
             tabPanel("Data")
  )
    
  )
)
