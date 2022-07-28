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
# Make categorical columns into factors
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

########### Make a special df for graphing ########### 
df <- data %>%
  mutate(hypertension = ifelse(hypertension == 0, "No","Yes")) %>%
  mutate(heart_disease = ifelse(heart_disease == 0, "No","Yes")) %>%
  mutate(stroke = ifelse(stroke == 0, "No","Yes")) 
  df$hypertension <- factor(df$hypertension) 
  df$heart_disease <- factor(df$heart_disease) 
  df$stroke <- factor(df$stroke) 

  ###########  Make some dummy variables ########### 
# gender, work_type, Residence_type, smoking_status
dummies <- dummyVars(age ~ ., data = data)
addDummy <- data.frame(predict(dummies, newdata = data))
# keep the columns I want
data2 <- data.frame(cbind(data[,c(1:5, 8:9,11)], addDummy[,c(8:14, 17:20)]))
# Reorder columns so stroke is last 
columnOrder <- c(1:7, 9:19, 8)
data2 <- data2[, columnOrder]


###########  Define server logic ########### 
shinyServer(function(input, output) {
  
  
  ###########  Data exploration tab ########### 
  output$graph <- renderPlot({
    
    ###########  Bar graph ########### 
    if(input$plotType == "bar"){
      df0 <- df %>%
        group_by(!!as.symbol(input$bars), stroke) %>% count(stroke) 
        ggplot(df0, aes(x = !!as.symbol(input$bars), y = n, fill = stroke)) +
        geom_bar(stat = "identity", position = "fill") +
        labs(title = paste0("Percentage of Observations with ",
                            str_to_title(input$bars),
                            " vs. without ",
                            str_to_title(input$bars)), 
             x = str_to_title(input$bars), 
             y = "Percentage") +
        scale_fill_brewer(palette = "Paired")
    }
    
    ###########  Scatterplot  ########### 
    else if(input$plotType == "scatterplot"){
      df1 <- df  %>% group_by(age, !!as.symbol(input$points), stroke) %>% 
        count(stroke) 
      ggplot(df1, aes(age, n, color = !!as.symbol(input$points))) +
        geom_point() +
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
  
  ###########  0. Splitting the data ########### 
  modeling <- eventReactive(input$go, {
    
    # Create a progress bar (not actually showing up, though)
    withProgress(message = "Fitting Models",
                 detail = 'Splitting Data', value = 0, {
                   
    set.seed(89)
    strokeIdx <- createDataPartition(y = data2$stroke,
                                     p = input$splitPct,
                                     list = FALSE)
    training <- data2[strokeIdx, ]
    testing <- data2[-strokeIdx, ]

    #########  1. Logistic Regression ######### 
    
    # Increment the progress bar, and update the detail text.
    setProgress(1/4, detail = "Logistic Regression")
    
    # Subset user-selected variables to use in models
    modelingData <- training[,c(input$lrVars, "stroke")]
    # Train model
    lrFit <- train(stroke ~ ., 
                     data = modelingData, 
                     method = "glm",
                     family = "binomial",
                     preProcess = c("center", "scale"),
                     trControl = trainControl(method = "cv", number = 5))
    output$lrSummary <- renderPrint({
      summary(lrFit)
    })
    ###########  2. Classification Tree ########### 
    
    # Increment the progress bar, and update the detail text.
    setProgress(2/4, detail = "Classification Tree")
    
  cTree <- train(stroke ~ .,
                 data = modelingData,
                 method = "rpart",
                 preProcess = c("center", "scale"),
                 cp = 0.001,
                 trControl = trainControl(method = "cv", 
                                          number = 5))
  output$ctreeSummary <- renderPrint({
    summary(cTree)
  })
  ###########  3. Random Forest ########### 
  
  # Increment the progress bar, and update the detail text.
  setProgress(3/4, detail = "Random Forest Model")
  
  # set up the mtry parameter 
  tunegrid <- expand.grid(.mtry=c(1:input$mtry)) # Update as user selects
  
  #train model
  rfFit <- train(stroke ~ .,
                 data = modelingData,
                 method = "rf",
                 tuneGrid = tunegrid,
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "cv", 
                                          number = 5))
  output$rfSummary <- renderPrint({
    summary(rfFit)
  })
  # Feature importance for random forest
  output$featureImport <- renderPrint({ 
    varImp(rfFit, scale = FALSE)
  })
  
  # Get RMSEs
  lrRMSE <- RMSE(predict(lrFit, newdata = testing), testing$stroke)
  ctRMSE <- RMSE(predict(cTree, newdata = testing), testing$stroke)
  rfRMSE <- RMSE(predict(rfFit, newdata = testing), testing$stroke)
  
  # Table of RMSEs
  output$rmse <- renderDT({
    data.frame(LogRegression = lrRMSE,
               ClassTree = ctRMSE,
               RandomForest = rfRMSE)
  })
  c(lrFit, cTree, rfFit)
  
    })
  }) # end go-button eventreactive} 
  observe(modeling)
  
  ####### Making predictions #######
  # Update based on user selected variables
  observeEvent(input$goPredict, { 
    predVars <- data.frame(gender = input$gender, 
                           age = input$age, 
                           hypertension = input$hypertension, 
                           heart_disease = input$heart_disease, 
                           ever_married = input$ever_married, 
                           work_type = input$work_type, 
                           Residence_type = input$Residence_type, 
                           avg_glucose_level = input$avg_glucose_level, 
                           bmi = input$bmi, 
                           smoking_status = input$smoking_status)
    # predict()
    if(input$modelForPred == "lr"){ 
      model <- modeling()[[1]]
      prediction <- predict(model, newdata = predVars)
    }
    else if(input$predModel == "ct"){ 
      model <- modeling()[[2]]
      prediction <- predict(model, newdata = predVars)
    }
    else { 
      model <- modeling()[[3]]
      prediction <- predict(model, newdata = predVars)
    }
    
    output$prediction <- renderPrint({ 
      prediction 
    })
  })
})

