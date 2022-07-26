library(caret) 
library(DT)
library(tidyverse)
library(randomForest)

################# Read in and clean the data #################
data <- readr::read_csv(file = "./strokeData.csv",
                        show_col_types = FALSE)

# Make categorical columns into factors
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


############# About Page #################
########## Data Exploration page ##########
# make a numerical summary that changes type of summary (mean/median)
DT::datatable({
  var <- "age" # choices for input$var are age, avg_glucose_level, bmi
  summType <- "mean" # choices for input$summType are mean, median
  round <- 2
  data <- data[, c("gender", "ever_married", var), drop = FALSE]
  tab <- aggregate(data[[var]] ~ gender + ever_married, data = data, FUN = summType)
  tab[, 3] <- round(tab[, 3], round)
  names(tab)[3] <- paste(str_to_title(summType), str_to_title(var), sep = " ")
  tab
})
#### make a graph that changes type (scatter / histogram or bar?) ######
plotType <- scatter # choices for input$plotType are scatter, bar
# If the user selects scatter, they can choose alpha for points to be 
### heart_disease or ever_married
output$strokePlot <- renderPlot({

  # Logic to set point aesthetics based on checkboxes
  if(input$plotType == "scatter"){
    g <- ggplot(data, aes(x = age, y = bmi))
    if(input$heart_disease){
      g + geom_point(size = input$size, 
                     aes(col = gender,
                         alpha = heart_disease))
    } else {
      g + geom_point(size = input$size, 
                     aes(col = gender),
                     alpha = ever_married)
    }
    # If the user chooses bar graph, they can choose from 
    ## a bunch of variables for x
  } else {
    xAxis <- "gender" # input$xAxis choices are work_type, residence,etc.
    g <- ggplot(data)
    g + geom_bar(mapping = aes(x = xAxis,fill=stroke))
  }
})

########### Modeling Page ##############
##### tab1 Modeling Info #########
# Explain 3 modeling approaches: benefits, drawbacks. Use mathtype (mathJax)




##### tab2 Model Fitting ########
# split data into training /test set
proportion <- 0.8 # this will be input$proportion from sliderInput 0.6 to 0.9
set.seed(89)
strokeIdx <- createDataPartition(y = data$stroke, p = proportion, list = FALSE)
training <- data[strokeIdx, ]
testing <- data[-strokeIdx, ]

# User chooses model setting for each model: variables used (checkboxInput)
# User presses a button when ready to fit all 3 models (submitButton)
# Model 1 is linear regression
regFit <- train(stroke ~ var1 + var2 + var3, # need to get vars from input$
              data = data[1:11], 
              method = "lm", # linear regression
              preProcess = c("center", "scale"),
              trControl = trainControl(method = "cv", number = 5)
)
# Model 2 is classification tree
rpartFit <- train(x = training[,1:10], 
                  y = training[,11],
                  method = "rpart",
                  preProcess = c("center", "scale"),
                  cp = 0.001,
                  trControl = trainControl(method = "repeatedcv", 
                                           number = 5, repeats = 3)
)
# Model 3 is Random Forest
# set up the mtry parameter 
tunegrid <- expand.grid(.mtry=c(1:9))
#train model
rfFit <- train(x = training[,1:10], 
               y = training[,11],
               method = "rf",
               tuneGrid = tunegrid,
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "repeatedcv", 
                                        number = 5, repeats = 3)
)

# Output fit stats like RMSE & summary(), feature importance for each model


##### tab3 Prediction ########
# User can choose 1 of 3 models for prediction
# They select the values of the predictors and obtain a prediction for the response



