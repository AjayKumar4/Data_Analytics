install.packages("plyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caTools")
install.packages("ROCR")
install.packages("pROC")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("psych")
install.packages("readxl")

installed.packages("plyr")
installed.packages("dplyr")
installed.packages("ggplot2")
installed.packages("caTools")
installed.packages("ROCR")
installed.packages("pROC")
installed.packages("rattle")
installed.packages("rpart.plot")
installed.packages("RColorBrewer")
installed.packages("psych")
installed.packages("readxl")

library(plyr)
library(dplyr)
library(ggplot2)
library(caTools) # for sample splitting
library(ROCR) # for the ROC/ AUC measure
library(pROC) # for the ROC/ AUC measure
library(rattle) # Visualization of Decision Trees
library(rpart.plot)
library(RColorBrewer)
library(psych)
library(readxl)

#setting Workspace path
setwd("~/workspace/Data_Analytics")

# Importing the dataset
dataset <- read_xlsx('Employeeattrition.xlsx')
employee_dataset <-  as.data.frame(unclass(dataset))
employee_dataset$Education <- factor(employee_dataset$Education,
                         levels = c(1, 2, 3, 4, 5),
                         labels = c('Below College', 'College', 'Bachelor', 'Master', 'Doctor'))
employee_dataset$EnvironmentSatisfaction <- factor(employee_dataset$EnvironmentSatisfaction,
                                     levels = c(1, 2, 3, 4),
                                     labels = c('Low', 'Medium', 'High', 'Very High'))
employee_dataset$JobInvolvement <- factor(employee_dataset$JobInvolvement,
                                                   levels = c(1, 2, 3, 4),
                                                   labels = c('Low', 'Medium', 'High', 'Very High'))
employee_dataset$JobSatisfaction <- factor(employee_dataset$JobSatisfaction,
                                          levels = c(1, 2, 3, 4),
                                          labels = c('Low', 'Medium', 'High', 'Very High'))
employee_dataset$PerformanceRating <- factor(employee_dataset$PerformanceRating,
                                           levels = c(1, 2, 3, 4),
                                           labels = c('Low', 'Good', 'Excellent', 'Outstanding'))
employee_dataset$RelationshipSatisfaction <- factor(employee_dataset$RelationshipSatisfaction,
                                           levels = c(1, 2, 3, 4),
                                           labels = c('Low', 'Medium', 'High', 'Very High'))
employee_dataset$WorkLifeBalance <- factor(employee_dataset$WorkLifeBalance,
                                             levels = c(1, 2, 3, 4),
                                             labels = c('Bad', 'Good', 'Better', 'Best'))

str(employee_dataset)
describe(employee_dataset)
summary(employee_dataset)

#employeeCount, standard hours

