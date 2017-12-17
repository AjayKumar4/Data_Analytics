install.packages("plyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caTools")
install.packages("ROCR")
install.packages("pROC")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("rpart")
install.packages("RColorBrewer")
install.packages("psych")
install.packages("readxl")
install.packages('e1071')
install.packages('caret')
install.packages("lattice")
install.packages("utils")  
install.packages("mlbench")
install.packages('abind')
install.packages('zoo')
install.packages('TTR')
install.packages('xts')
install.packages('quantmod')
install.packages('ROCR')
install.packages("curl")
install.packages("DMwR")
install.packages("kernlab")
install.packages("randomForest")
install.packages("xgboost")
install.packages("caretEnsemble")

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
installed.packages('e1071')
installed.packages("caret")
installed.packages("lattice")
installed.packages("utils")  
installed.packages("mlbench")
installed.packages("DMwR")
installed.packages("kernlab")
installed.packages("randomForest")
installed.packages("xgboost")
installed.packages("caretEnsemble")

library(plyr)
library(dplyr)
library(ggplot2)
library(caTools) # for sample splitting
library(ROCR) # for the ROC/ AUC measure
library(pROC) # for the ROC/ AUC measure
library(rattle) # Visualization of Decision Trees
library(rpart.plot)
library(rpart)
library(RColorBrewer)
library(psych)
library(readxl)
library(lattice)
library(utils)    #for importing the file
library(mlbench)  #for correlation matrix
library(caret) #for models
library(DMwR)
library(kernlab) #SVMRadial
library(randomForest) # RandomForest
library(xgboost)
library(caretEnsemble)


#setting Workspace path
setwd("~/Workspace/Data_Analytics")

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

#scatter plot between monthly income, work life balance and attrition
employee_dataset %>%
  ggplot(aes(x = MonthlyIncome,y = WorkLifeBalance, color = Attrition)) + 
  geom_point(aes(y = WorkLifeBalance, fill = Attrition), 
           stat="identity", 
           alpha = 0.7) +
  scale_colour_manual(values = c('yellow','red'))+
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Monthly Income VS Work Life Balance VS Attrition")

#scatter plot between monthly income, JobLevel and attrition
employee_dataset %>%
  ggplot(aes(x = MonthlyIncome,y = JobLevel, color = Attrition)) + 
  geom_point(aes(y = JobLevel, fill = Attrition), 
             stat="identity", 
             alpha = 0.7) +
  scale_colour_manual(values = c('yellow','red'))+
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Monthly Income VS JobLevel VS Attrition")

#Find highly correlated features (optional)
correlation_matrix=cor(employee_dataset[sapply(employee_dataset, is.numeric)])
highlyCorrelated = findCorrelation(correlation_matrix, cutoff=0.6)
highlyCorrelated

#Step:4 Define train control for models using method as "repeatedcv"(repeated K-fold cross-validation)
train_control=trainControl(method="repeatedcv", number=5, repeats=3)


#Implementing ML Models for Prediction


#train the mode using k-Nearest Neighbors
model_knn=train(Attrition~., employee_dataset, method="knn", trControl=train_control)

# estimate variable importance
varImp(model_knn)
importance_knn=varImp(model_knn, scale=FALSE)
importance_knn

plot(importance_knn)
# select the top-ranking variables.
# drop the low ranking variables. Here the last 10 variables are dropped. 

importance_knn_list <- rownames(importance_knn$importance)[order(importance_knn$importance$Yes, decreasing=TRUE)[1:20]]
importance_knn_list

# drop the low ranking variables. Here the last 10 variables are dropped. 
#top_var <- 
#  importance_knn_list[1:((ncol(employee_dataset)-1) - 10)] %>%
#  as.character() 
#top_var

# select the top ranking variables 

employee_dataset <-employee_dataset %>% select(., one_of(c(importance_knn_list, "Attrition")))

#Resampling

train_index <- 
  createDataPartition(employee_dataset$Attrition,
                      times=1,
                      p=.7) %>%
  unlist()


employee_dataset_train <- employee_dataset[train_index, ]
employee_dataset_test <- employee_dataset[-train_index, ]

prop.table(table(employee_dataset_train$Attrition))
employee_dataset_train %>%
  ggplot(aes(x= Attrition,  fill=Attrition)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Count", fill="day") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

#Certainly Working Employee are 864 and Employees who left the company is 166
#Data imbalance because working employee data is 83.9% and left employee data is 16.1% 
#which is very less, Hence Upscaling the Data

employee_dataset_train <- as.data.frame(employee_dataset_train)

employee_dataset_train <- SMOTE(Attrition ~ .,
                                employee_dataset_train,
                                perc.over=300,
                                perc.under=150)

prop.table(table(employee_dataset_train$Attrition))
employee_dataset_train %>%
  ggplot(aes(x= Attrition,  fill=Attrition)) + 
  geom_bar(aes(y = ..count.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Count", fill="day") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")
#After Upscaling working employee data is 52.94% and left employee data is 47.06% 

#Model Building

# initialize training control. 
tc <- trainControl(method="boot", 
                   number=3, 
                   repeats=3, 
                   search="grid",
                   classProbs=TRUE,
                   savePredictions="final",
                   summaryFunction=twoClassSummary)

# SVM model.

time_svm <- system.time(
  model_svm <- train(Attrition ~ .,
                     employee_dataset_train,
                     method="svmLinear",
                     trainControl=tc)
)

# random forest model

time_rf <- system.time(
  model_rf <- train(Attrition ~ .,
                    employee_dataset_train,
                    method="rf",
                    trainControl=tc)
)

# xgboost model.

time_xgb <- system.time(
  model_xgb <- train(Attrition ~ .,
                     employee_dataset_train,
                     method="xgbLinear",
                     trainControl=tc)
)

# ensemble of the three models.

time_ensemble <- system.time(
  model_list <- caretList(Attrition ~ ., 
                          data=employee_dataset_train,
                          trControl=tc,
                          methodList=c("svmLinear", "rf", "xgbLinear"))
)

# stack of models. Use glm for meta model.

model_stack <- caretStack(
  model_list,
  metric="ROC",
  method="glm",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)

#Model Validation
models_list <- list(model_svm, model_rf, model_xgb, model_stack)

predictions <-lapply(models_list, 
                     predict, 
                     newdata=select(employee_dataset_test, -Attrition))
#predictions <-predict(models_list, newdata=select(employee_dataset_test, -Attrition))

# confusion matrix evaluation results.

cm_metrics <- lapply(predictions,
                     confusionMatrix, 
                     reference=employee_dataset_test$Attrition, 
                     positive="Yes")

# accuracy

acc_metrics <- 
  lapply(cm_metrics, `[[`, "overall") %>%
  lapply(`[`, 1) %>%
  unlist()

# recall

rec_metrics <- 
  lapply(cm_metrics, `[[`, "byClass") %>%
  lapply(`[`, 1) %>%
  unlist()

# precision

pre_metrics <- 
  lapply(cm_metrics, `[[`, "byClass") %>%
  lapply(`[`, 3) %>%
  unlist()

algo_list <- c("SVM", "Random Forest", "Xgboost", "Stacking")
time_consumption <- c(time_svm[3], time_rf[3], time_xgb[3], time_ensemble[3])

df_comp <- 
  data.frame(Models=algo_list, 
             Accuracy=acc_metrics, 
             Recall=rec_metrics, 
             Precision=pre_metrics,
             Time=time_consumption) %>%
             {head(.) %>% print()}


#Designing a Decision Tree

#Fitting Decision Tree to the training dataset
set.seed(25)
time_dt <- system.time(
          dtModel <- rpart(formula = Attrition ~ ., 
                           data = employee_dataset_train, 
                           method = "class", 
                           control = rpart.control(minbucket = 15))
)

#Predicting Result on test set DT Model
y_pred_dt = predict(dtModel, newdata = employee_dataset_test, type = "class" )

#Making Confusion Matrix for Decision Tree
cm_metrics_dt_model <- confusionMatrix(y_pred_dt, employee_dataset_test$Attrition)

#Accuracy
acc_metrics <- cm_metrics_dt_model$overall[,1]

# recall
rec_metrics <- cm_metrics_dt_model$byClass[,1]

# precision
pre_metrics <- cm_metrics_dt_model$byClass[,3]

df_comp <- 
  data.frame(Models="Decision Tree", 
             Accuracy=acc_metrics[1], 
             Recall=rec_metrics[1], 
             Precision=pre_metrics[3],
             Time=time_dt[3]) %>%
             {head(.) %>% print()}
plot(dtModel)
text(dtModel)
printcp(dtModel)