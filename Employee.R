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

#employeeCount, standard hours, Over18,EmployeeNumber is not required for modelling 
#since data does not mean anything to employee attribution column

attrition_table <- table(dataset$Attrition)
percentlabels<- round(100*attrition_table/sum(attrition_table),2)
pielabels<- paste(percentlabels, "%", sep="")
pie(attrition_table, labels=pielabels,main="Employee Attrition",col = c("green", "red"))
legend("topright", names(attrition_table), cex=0.8, fill=c("green", "red"))

employee_dataset %>%
ggplot(aes(x= Attrition,  fill=Attrition)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Count", fill="day") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")


# Removing employeeCount, standard hours, Over18,EmployeeNumber columns(non-significant columns)

employee_dataset$EmployeeNumber <- NULL
employee_dataset$EmployeeCount <- NULL
employee_dataset$EmployeeNumber <- NULL
employee_dataset$StandardHours <- NULL
employee_dataset$Over18 <- NULL

str(employee_dataset)
describe(employee_dataset)
summary(employee_dataset)

#Visulaiztion Considering Attrition of Employee

#V1: Attrition VS Overtime
employee_dataset %>%
  ggplot(aes(x = OverTime, group = Attrition)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Count", fill= "over time") +
  facet_grid(~Attrition) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  ggtitle("Attrition VS Overtime",subtitle = "Attrition")

#V2: BusinessTravel VS Attrition
employee_dataset %>%
  ggplot(aes(x = BusinessTravel, group = Attrition)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Count", fill= "business travel") +
  facet_grid(~Attrition) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  ggtitle("BusinessTravel VS Attrition",subtitle = "Attrition")

#V3: Marital status VS Attrition
employee_dataset %>%
  ggplot(aes(x = MaritalStatus, group = Attrition)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Count", fill= "marital status") +
  facet_grid(~Attrition) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  ggtitle("Marital status VS Attrition",subtitle = "Attrition")

#V4: Job role Vs Attrition
levels(employee_dataset$JobRole) <- c("HC Rep", "HR", "LT", "Man", "MD", "RD", "RsScientist", "SalesEx", "SalesRep")
employee_dataset %>%
  ggplot(aes(x = JobRole, group = Attrition)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Count", fill= "job role") +
  facet_grid(~Attrition) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  ggtitle("Job role VS Attrition",subtitle = "Attrition")



