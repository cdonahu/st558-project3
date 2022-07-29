library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(rpart)
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

###########  Make some dummy variables ########### 
# gender, work_type, Residence_type, smoking_status
dummies <- dummyVars(age ~ ., data = data)
addDummy <- data.frame(predict(dummies, newdata = data))
# keep the columns I want
data2 <- data.frame(cbind(data[,c(1:5, 8:9,11)], addDummy[,c(8:14, 17:20)]))
# Reorder columns so stroke is last 
columnOrder <- c(1:7, 9:19, 8)
data2 <- data2[, columnOrder]

############# About Page #################
########## Data Exploration page ##########


########### Modeling Page ##############
##### tab1 Modeling Info #########
# Explain 3 modeling approaches: benefits, drawbacks. Use mathtype (mathJax)


##### tab2 Model Fitting ########

# User chooses model setting for each model: variables used (checkboxInput)
# Subset user-selected variables to use in models
fitVars <- c("age", "bmi", "hypertension", "heart_disease", "avg_glucose_level", "gender", "work_type")
modelingData <- data[,c(fitVars, "stroke")] # problem with a categorical column

# split data into training /test set
splitPct <- 0.8 # input$splitPct 
set.seed(89)
strokeIdx <- createDataPartition(y = data2$stroke, p = splitPct, list = FALSE)
training <- modelingData[strokeIdx, ]
testing <- modelingData[-strokeIdx, ]

# User presses a button when ready to fit all 3 models (submitButton)
# Model 1 is logistic regression
lrFit <- train(stroke ~ ., 
               data = training, 
               method = "glm",
               family = "binomial",
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv", number = 5))
# Model 2 is classification tree
cTree <- train(stroke ~.,
                  data = training,
                  method = "rpart",
                  preProcess = c("center", "scale"),
                  cp = 0.001,
                  trControl = trainControl(method = "cv", 
                                           number = 5)
)

paste(capture.output(cTree), collapse = "<br>")

# Model 3 is Random Forest
# set up the mtry parameter 
tunegrid <- expand.grid(.mtry=c(1:4))
#train model
rfFit <- train(stroke ~ .,
               data = training,
               method = "rf",
               tuneGrid = tunegrid,
               preProcess = c("center", "scale"),
               trControl = trainControl(method = "cv", 
                                        number = 5)
)

varImp(rfFit, scale = FALSE)

accuracy <- datatable(round(data.frame("Logistic Regression" = lrFit$results$Accuracy,
                     "Classification Tree" = mean(cTree$results$Accuracy),
                     "Random Forest" = mean(rfFit$results$Accuracy)), 3))


# Confusion matrix to print
confusionMatrix(predict(lrFit, newdata = testing), testing$stroke)
confusionMatrix(predict(cTree, newdata = testing), testing$stroke)
confusionMatrix(predict(rfFit, newdata = testing), testing$stroke)

##### tab3 Prediction ########
# User can choose 1 of 3 models for prediction
# They select the values of the predictors and obtain a prediction for the response



