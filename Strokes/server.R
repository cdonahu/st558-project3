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

########### Make a special df for graphing ########### 
df <- data %>%
  mutate(hypertension = ifelse(hypertension == 0, "No","Yes")) %>%
  mutate(heart_disease = ifelse(heart_disease == 0, "No","Yes")) %>%
  mutate(stroke = ifelse(stroke == 0, "No","Yes")) 
  df$hypertension <- factor(df$hypertension) 
  df$heart_disease <- factor(df$heart_disease) 
  df$stroke <- factor(df$stroke) 

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
  ###########  Data Table  ###########  
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
  ###########  MathJax   ########### 
  
  
  ###########  0. Splitting the data ########### 
  modeling <- eventReactive(input$go, {
    
    # Subset user-selected variables to use in models
    modelingData <- data[,c(input$fitVars, "stroke")] 
    
    # Create a progress bar 
    withProgress(message = "Fitting Models",
                 detail = 'Splitting Data', value = 0, {
                   
    set.seed(89)
    strokeIdx <- createDataPartition(y = data2$stroke,
                                     p = input$splitPct,
                                     list = FALSE)
    training <- modelingData[strokeIdx, ]
    testing <- modelingData[-strokeIdx, ]

    #########  1. Logistic Regression ######### 
    
    # Increment the progress bar, and update the detail text.
    setProgress(1/6, detail = "Logistic Regression")
    
    # Train model
    lrFit <- train(stroke ~ ., 
                     data = training, 
                     method = "glm",
                     family = "binomial",
                     preProcess = c("center", "scale"),
                     trControl = trainControl(method = "cv", number = 5))
    # Output a summary of the model to print out to user
    output$lrSummary <- renderPrint({
      lrFit$finalModel
    })
    ###########  2. Classification Tree ########### 
    
    # Increment the progress bar, and update the detail text.
    setProgress(2/6, detail = "Classification Tree")
    
  cTree <- train(stroke ~ .,
                 data = training,
                 method = "rpart",
                 preProcess = c("center", "scale"),
                 cp = 0.001,
                 trControl = trainControl(method = "cv", 
                                          number = 5))

  # Print classification tree somehow for output
  output$ctreeSummary <- renderPrint({ 
    cTree
  })
  ###########  3. Random Forest ########### 
  
  # Increment the progress bar, and update the detail text.
  setProgress(3/6, detail = "Random Forest Model")
  
  # set up the mtry parameter 
  tunegrid <- expand.grid(.mtry=c(1:input$mtry)) # Update as user selects
  
  #train model
  rfFit <- train(stroke ~ .,
                 data = training,
                 method = "rf",
                 tuneGrid = tunegrid,
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "cv", 
                                          number = 5))
  # Increment the progress bar, and update the detail text.
  setProgress(4/6, detail = "Evaluating Models")
  
  # Feature importance for random forest
  output$featureImport <- renderPrint({ 
    varImp(rfFit, scale = FALSE)
  })
  
  # Get Accuracy on training data
  output$accuracy <- renderDT({ 
    datatable(round(data.frame("Logistic Regression" = lrFit$results$Accuracy,
                               "Classification Tree" = mean(cTree$results$Accuracy),
                               "Random Forest" = mean(rfFit$results$Accuracy)), 3))
    })
  
  # Increment the progress bar, and update the detail text.
  setProgress(5/6, detail = "Generating Table")
  
  # Confusion Matrix from testing data for each model to output
  output$lrConfusionMatrix <- renderPrint({ 
    confusionMatrix(predict(lrFit, newdata = testing), testing$stroke)
  })
  output$ctConfusionMatrix <- renderPrint({ 
    confusionMatrix(predict(cTree, newdata = testing), testing$stroke)
  })
  output$rfConfusionMatrix <- renderPrint({ 
    confusionMatrix(predict(rfFit, newdata = testing), testing$stroke)
  })
  
  
  }) # end progress bar 
    
  c(lrFit, cTree, rfFit)
                 }) # end go-button eventreactive} 

  observe(modeling())
  
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
    
    # Subset user-selected variables to use in models
    predData <- predVars[,c(input$fitVars)]
    
    # predict() (Warning: Error in [[: object of type 'symbol' is not subsettable)
    modelChoice <- input$modelForPred
    modelChoice <- as.name(modelChoice)
    prediction <- caret::predict.train(modelChoice, newdata = predData)
    
    output$prediction <- renderPrint({ 
      prediction 
    }) # end renderprint
  }) # end prediction events
  
  ##### Data Tab ####
  # Get users selected columns
  dataTab <- reactive({
    data[,c(input$dataToSave)]
  })
  
  # Save as an output object to render
  observe({
    output$dtable <- renderDT({
      dataTab()
    })
  })
  
  # Allow user to export as CSV file
  observeEvent(input$goSave, {
    usersExportFile <- dataTab()
    write_csv(usersExportFile, "Stroke_Data_for_User.csv")
  })

})

