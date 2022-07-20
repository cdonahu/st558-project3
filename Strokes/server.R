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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$summary <- DT::renderDataTable({
    var <- input$var
    GermanCreditSub <- GermanCredit[, c("Class", "InstallmentRatePercentage", var), drop = FALSE]
    tab <- aggregate(GermanCreditSub[[var]] ~ Class + InstallmentRatePercentage, data = GermanCreditSub, FUN = mean)
    tab[, 3] <- round(tab[, 3], input$round)
    names(tab)[3] <- paste0("Average ", var)
    tab
  })
  
  output$barPlot <- renderPlot({
    
    g <- ggplot(GermanCredit, aes(x = Class))  
    
    if(input$plot == "bar"){
      g + geom_bar()
    } else if(input$plot == "sideUmemploy"){ 
      g + geom_bar(aes(fill = as.factor(EmploymentDuration.Unemployed)), position = "dodge") + scale_fill_discrete(name = "Unemployment status", labels = c("Employed", "Unemployed"))
    } else if(input$plot == "sideForeign"){
      g + geom_bar(aes(fill = as.factor(ForeignWorker)), position = "dodge") + scale_fill_discrete(name = "Status", labels = c("German", "Foreign"))
    }
  })
  
})
