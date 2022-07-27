library(caret) 
library(DT)
library(tidyverse)
library(randomForest)

################# Read in and clean the data #################
data <- readr::read_csv(file = "./strokeData.csv",
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


############# About Page #################
########## Data Exploration page ##########


########### Modeling Page ##############
##### tab1 Modeling Info #########
# Explain 3 modeling approaches: benefits, drawbacks. Use mathtype (mathJax)




##### tab2 Model Fitting ########
# split data into training /test set
splitPct <- 0.8 # input$splitPct 
set.seed(89)
strokeIdx <- createDataPartition(y = data$stroke, p = splitPct, list = FALSE)
training <- data[strokeIdx, ]
testing <- data[-strokeIdx, ]

# User chooses model setting for each model: variables used (checkboxInput)
# User presses a button when ready to fit all 3 models (submitButton)
# Model 1 is logistic regression
lrFit <- train(stroke ~ age + bmi, # Update as user selects
               data = training[1:11], 
               method = "glm",
               family = "binomial",
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv", number = 5))
# Model 2 is classification tree
rpartFit <- train(x = training[,1:10], 
                  y = training$stroke,
                  method = "rpart",
                  preProcess = c("center", "scale"),
                  cp = 0.001,
                  trControl = trainControl(method = "cv", 
                                           number = 5)
)
# Model 3 is Random Forest
# set up the mtry parameter 
tunegrid <- expand.grid(.mtry=c(1:9))
#train model
rfFit <- train(x = training[,1:10], 
               y = training$stroke,
               method = "rf",
               tuneGrid = tunegrid,
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv", 
                                        number = 5)
)

# Output fit stats like RMSE & summary(), feature importance for each model


##### tab3 Prediction ########
# User can choose 1 of 3 models for prediction
# They select the values of the predictors and obtain a prediction for the response



