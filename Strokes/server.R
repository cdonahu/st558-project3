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
# Make gender/ married columns into factors
data$gender <- factor(data$gender)
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
data <- data.frame(cbind(data[,c(1:5, 8:9,11)], addDummy[,c(8:14, 17:20)]))
# Reorder columns so stroke is last 
columnOrder <- c(1:7, 9:19, 8)
data <- data[, columnOrder]


# Define server logic 
shinyServer(function(input, output) {
  
  
# Data exploration tab
  output$graph <- renderPlot({
    df <- data %>% group_by(input$var, stroke) %>% count(stroke)
    # Line graph
    if(input$plotType == "line"){
      ggplot(df) + geom_line(x = input$var, y = n)
    }
    # Bar graph
    else if(input$plotType == "bar"){
      xAxis <- "gender" # input$xAxis choices are work_type, residence,etc.
      g <- ggplot(data)
      g + geom_bar(mapping = aes(x = xAxis,fill=stroke))
    }
    # Scatterplot
    else if(input$plotType == "scatterplot"){
      df <- data %>% group_by(age, stroke) %>% count(stroke)
      g <- ggplot(df, aes(x = age, y = n))
      if(input$age){
        g + geom_point(size = input$size, 
                       aes(col = stroke,
                           alpha = 0.5))
      } else {
        g + geom_point(size = input$size, 
                       aes(col = gender),
                       alpha = ever_married)
      }
    }
  })
  
  output$summary <- DT::renderDataTable({
    round <- 2
    data <- data[, c("gender", "ever_married", input$var), drop = FALSE]
    tab <- aggregate(data[[input$var]] ~ gender + ever_married, 
                     data = data, 
                     FUN = input$summType)
    tab[, 3] <- round(tab[, 3], round) #input$round)
    names(tab)[3] <- paste(str_to_title(summType), str_to_title(input$var), sep = " ")
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
