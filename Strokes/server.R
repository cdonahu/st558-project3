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


# Define server logic 
shinyServer(function(input, output) {
  
  
# Data exploration tab
  output$graph <- renderPlot({
    
    # Bar graph Error Continuous value supplied to discrete scale
    if(input$plotType == "bar"){
      df1 <- df %>%
        group_by(input$bars, stroke) %>% # hypertension or heart_disease
        count(stroke)
      df1 %>%
        ggplot(mapping = aes(x = input$bars, y = n, fill = stroke) ) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = paste0("Percentage of subjects with ", str_to_title(input$bars), " vs. without ", str_to_title(input$bars)), 
             x = str_to_title(input$bars), 
             y = "Percentage") +
        scale_fill_brewer(palette = "Paired")
    }
    
    # Scatterplot 
    else if(input$plotType == "scatterplot"){
      df2 <- data %>% group_by(age, input$points, stroke) %>% count(stroke)
      ggplot(df2, aes(x = age, y = n, color = input$points)) +
        labs(title = paste0("Age Distribution by ", str_to_title(input$points)),
             x = "Age", 
             y = "Number of Observations")
    }
  })
  
  output$summary <- DT::renderDT({
    round <- 2
    df3 <- data[, c("gender", "ever_married", input$var), drop = FALSE]
    tab <- aggregate(df3[[input$var]] ~ gender + ever_married, 
                     data = df3, 
                     FUN = input$summType)
    tab[, 3] <- round(tab[, 3], round) 
    names(tab)[3] <- paste(str_to_title(input$summType), str_to_title(input$var), sep = " ")
    tab
  })
  
  # 0. Splitting the data
  eventReactive(input$go, {
    set.seed(89)
    strokeIdx <- createDataPartition(y = data2$stroke,
                                     p = input$splitPct,
                                     list = FALSE)
    training <- data2[strokeIdx, ]
    testing <- data2[-strokeIdx, ]

  # 1. Logistic Regression 
    lrFit <- train(stroke ~ age + bmi, # Update as user selects
                     data = training[1:11], 
                     method = "glm",
                     family = "binomial",
                     preProcess = c("center", "scale"),
                     trControl = trainControl(method = "cv", number = 5))
  # 2. Classification Tree
  cTree <- train(x = training[,1:10], # Update as user selects
                 y = training[,11],
                 method = "rpart",
                 preProcess = c("center", "scale"),
                 cp = 0.001,
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 5, repeats = 3)
  )
  
  # 3. Random Forest 
  # set up the mtry parameter 
  tunegrid <- expand.grid(.mtry=c(1:10)) # Update as user selects
  
  #train model
  rfFit <- train(x = training[,1:10], # Update as user selects
                 y = training[,11],
                 method = "rf",
                 tuneGrid = tunegrid,
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 5, repeats = 3)
  )
  }) # end go-button eventreactive
})
