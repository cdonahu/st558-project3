library(caret) 

################# Read in and clean the data #################
data <- readr::read_csv(file = "./strokeData.csv",
                        show_col_types = FALSE)
# Make some married column into factors
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
data <- data.frame(cbind(data[,c(2:5, 8:9,11)], addDummy[,c(1,3,8:14, 17:20)]))
# Reorder columns so stroke is last 
columnOrder <- c(1:6, 8:20, 7)
data <- data[, columnOrder]






# About Page
# Data Exploration page
