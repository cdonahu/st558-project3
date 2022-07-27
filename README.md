# st558-project3
Final project for ST558 summer 2022


This project uses data on medical patients to predict strokes within an interactive app, allowing a user to explore, model, and look at predictions based on patients' characteristics. 

App users need the following packages installed before running the app:  
- tidyverse  
- shiny  
- caret  
- DT  
- rpart  
- randomForest

The following code will install the required packages:  
```
install.packages(c("tidyverse", "shiny", "caret", "DT", "rpart", "randomForest"))
```

The code below will allow the user to run the app from RStudio using `shiny::runGitHub()`:  
```
shiny::runGitHub("st558-project3", "cdonahu", subdir = "/Strokes")
```