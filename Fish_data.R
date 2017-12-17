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
install.packages("xlsx")
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
install.packages("magrittr")
install.packages("klaR")


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
installed.packages("xlsx")
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
installed.packages("magrittr")

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
library(xlsx)
library(lattice)
library(utils)    #for importing the file
library(mlbench)  #for correlation matrix
library(caret) #for models
library(DMwR)
library(kernlab) #SVMRadial
library(randomForest) # RandomForest
library(xgboost)
library(caretEnsemble)
library(magrittr)
library(tidyr)
library(klaR)


#setting Workspace path
setwd("~/Workspace/Data_Analytics")


# Importing the dataset
#economic_dataset <- read.csv('economic.csv', header=TRUE, sep=",", stringsAsFactors=FALSE, fileEncoding="latin1", na.strings=c("","NA"))
#fish_dataset <- read.csv('fish2.csv', header=TRUE, sep=",", stringsAsFactors=FALSE, fileEncoding="latin1", na.strings=c("","NA"))
economic_dataset <- read.csv('economic.csv', header=TRUE, sep=",", stringsAsFactors=TRUE, fileEncoding="latin1")
fish_dataset <- read.csv('fish2.csv', header=TRUE, sep=",", stringsAsFactors=TRUE, fileEncoding="latin1")
#fish_dataset <- read_xlsx('Book4.xlsx')

#economic_colnames <-colnames(economic_dataset)
#fish_colnames <-colnames(fish_dataset)
#Correcting column name so that economic_dataset and fish_dataset column are same
economic_colnames <-gsub("\\.", "", colnames(economic_dataset))
fish_colnames <- gsub("\\.", "", colnames(fish_dataset))
#economic_colnames <-gsub("Vessel  ID", "VesselID", economic_colnames)
fish_colnames <- gsub("ïvid", "VesselID", fish_colnames)
names(economic_dataset) <- economic_colnames
names(fish_dataset) <- fish_colnames

#merging economic_dataset and fish_dataset to fishing_dataset
fishing_dataset <- left_join(economic_dataset, fish_dataset, by =c("VesselID","Year"))


summary(fishing_dataset)
fishing_colnames <- colnames(fishing_dataset)

#removing Euro Symbol from the dataset
fishing_dataset$FishingIncome <-gsub("\u0080", "", fishing_dataset$FishingIncome)
fishing_dataset$NonFishingIncome <-gsub("\u0080", "", fishing_dataset$NonFishingIncome)
fishing_dataset$TotalIncome <-gsub("\u0080", "", fishing_dataset$TotalIncome)
fishing_dataset$Wages <-gsub("\u0080", "", fishing_dataset$Wages)
fishing_dataset$EnergyCostsFuel <-gsub("\u0080", "", fishing_dataset$EnergyCostsFuel)
fishing_dataset$RepairsMaintenance <-gsub("\u0080", "", fishing_dataset$RepairsMaintenance)
fishing_dataset$FiltersLubeOil <-gsub("\u0080", "", fishing_dataset$FiltersLubeOil)
fishing_dataset$Provisions <-gsub("\u0080", "", fishing_dataset$Provisions)
fishing_dataset$Ice <-gsub("\u0080", "", fishing_dataset$Ice)
fishing_dataset$DuesLevies <-gsub("\u0080", "", fishing_dataset$DuesLevies)
fishing_dataset$SundryVariableCosts <-gsub("\u0080", "", fishing_dataset$SundryVariableCosts)
fishing_dataset$TotalVariableCosts <-gsub("\u0080", "", fishing_dataset$TotalVariableCosts)
fishing_dataset$Insurance <-gsub("\u0080", "", fishing_dataset$Insurance)
fishing_dataset$LoanInterest <-gsub("\u0080", "", fishing_dataset$LoanInterest)
fishing_dataset$Accountancy <-gsub("\u0080", "", fishing_dataset$Accountancy)
fishing_dataset$LegalFees <-gsub("\u0080", "", fishing_dataset$LegalFees)
fishing_dataset$Sundryfixedcosts <-gsub("\u0080", "", fishing_dataset$Sundryfixedcosts)
fishing_dataset$TotalFixedCosts <-gsub("\u0080", "", fishing_dataset$TotalFixedCosts)
fishing_dataset$TOTALCOSTS <-gsub("\u0080", "", fishing_dataset$TOTALCOSTS)
fishing_dataset$GROSSPROFIT <-gsub("\u0080", "", fishing_dataset$GROSSPROFIT)
fishing_dataset$Depreciation <-gsub("\u0080", "", fishing_dataset$Depreciation)
fishing_dataset$Sundryreceipts <-gsub("\u0080", "", fishing_dataset$Sundryreceipts)
fishing_dataset$NetProfitLoss <-gsub("\u0080", "", fishing_dataset$NetProfitLoss)

#converting variable to Profit or loss or Break even
#fishing_dataset$NetProfitLoss <- ifelse(fishing_dataset$NetProfitLoss > 0, "Profit", 
#                                        ifelse(fishing_dataset$NetProfitLoss == 0,"Break even","loss"))

#Write new dataframe to XLSX to verify manually whether merge of data is proper
#write.xlsx(fishing_dataset, "df.xlsx")

fishing_dataset <- as.data.frame(unclass(fishing_dataset))

str(fishing_dataset)
describe(fishing_dataset)
summary(fishing_dataset)

#remove below feature in fishing Dataset due to NA(missing) Data
#valueS83,valueS9,valueS90,valueS81,valueS78,valueS70,valueS66,valueS64,valueS62,valueS61,valueS52,valueS50
#valueS4,valueS39,valueS36,valueS33,valueS31,valueS29,valueS28,valueS25,valueS20,valueS19,valueS13,valueS12
#valueS11,valueS1
fishing_dataset$valueS83 <- NULL
fishing_dataset$valueS9 <- NULL
fishing_dataset$valueS90 <- NULL
fishing_dataset$valueS81 <- NULL
fishing_dataset$valueS78 <- NULL
fishing_dataset$valueS70 <- NULL
fishing_dataset$valueS66 <- NULL
fishing_dataset$valueS64 <- NULL
fishing_dataset$valueS62 <- NULL
fishing_dataset$valueS61 <- NULL
fishing_dataset$valueS52 <- NULL
fishing_dataset$valueS50 <- NULL
fishing_dataset$valueS4 <- NULL
fishing_dataset$valueS39 <- NULL
fishing_dataset$valueS36 <- NULL
fishing_dataset$valueS33 <- NULL
fishing_dataset$valueS31 <- NULL
fishing_dataset$valueS29 <- NULL
fishing_dataset$valueS28 <- NULL
fishing_dataset$valueS25 <- NULL
fishing_dataset$valueS20 <- NULL
fishing_dataset$valueS19 <- NULL
fishing_dataset$valueS13 <- NULL
fishing_dataset$valueS12 <- NULL
fishing_dataset$valueS11 <- NULL
fishing_dataset$valueS1 <- NULL

str(fishing_dataset)
describe(fishing_dataset)
summary(fishing_dataset)

#removing ReferenceNumber which is unique in the dataset
fishing_dataset$ReferenceNumber <- NULL


#Visulaiztion Considering Attrition of Fish Dataset


#Find highly correlated features (optional)
#correlation_matrix <-cor(fishing_dataset[sapply(fishing_dataset, is.numeric)], use = "pairwise.complete.obs", method="kendall")
#highlyCorrelated = findCorrelation(correlation_matrix, cutoff=0.1)
correlation_matrix=cor(fishing_dataset[sapply(fishing_dataset, is.numeric)])
highlyCorrelated = findCorrelation(correlation_matrix, cutoff=0.6)
highlyCorrelated
#aj <- fishing_dataset[,-c(highlyCorrelated)]
#fishing_dataset <- fishing_dataset[,-c(highlyCorrelated)]
#summary(aj)

#Replace N/A value with Zero
#fishing_dataset[is.zero(fishing_dataset)] <- 0
fishing_dataset <- na.omit(fishing_dataset, cols = c("NetProfitLoss"))
fishing_dataset[fishing_dataset == ""] <- 0
fishing_dataset <- as.data.frame(sapply(fishing_dataset,gsub,pattern=",",replacement=""))
fishing_dataset <- as.data.frame(unclass(fishing_dataset))
fishing_dataset$Segment= as.integer(as.factor(fishing_dataset$Segment))
fishing_dataset$SizeCategory= as.integer(as.factor(fishing_dataset$Segment))

#Step:4 Define train control for models using method as "repeatedcv"(repeated K-fold cross-validation)
train_control=trainControl(method="repeatedcv", number=5, repeats=3)
#train_control=trainControl(method="boot", number=100)


#mean(fishing_dataset, na.rm=TRUE)
#fishing_dataset <- fishing_dataset[, colSums(is.na(fishing_dataset)) == 0]

#Implementing ML Models for Prediction
fishing_dataset$NetProfitLoss

#train the mode using k-Nearest Neighbors
#fishing_dataset <- na.omit(fishing_dataset, cols = c("NetProfitLoss"))
#fishing_dataset <- fishing_dataset[, colSums(is.na(fishing_dataset)) == 0]
#dat <- ifelse(fishing_dataset == NULL, mean(fishing_dataset, na.rm=TRUE), fishing_dataset)
#model_knn=train(NetProfitLoss~., fishing_dataset, method="knn", trControl=train_control)
#aj <- na.omit(fishing_dataset, cols = c("NetProfitLoss"))
#aj <- fishing_dataset %>% drop_na(NetProfitLoss)

# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(fishing_dataset$NetProfitLoss, p=split, list=FALSE)
data_train <- fishing_dataset[ trainIndex,]
data_test <- fishing_dataset[-trainIndex,]
split = sample.split(fishing_dataset$NetProfitLoss, SplitRatio = 0.8)
training_set = subset(fishing_dataset, split == TRUE)
test_set = subset(fishing_dataset, split == FALSE)


#Model Building

# initialize training control. 
tc <- trainControl(method="boot", 
                   number=3, 
                   #repeats=3, 
                   search="grid",
                   classProbs=TRUE,
                   savePredictions="final",
                   summaryFunction=twoClassSummary)


# SVM model.

time_svm <- system.time(
  model_svm <- train(NetProfitLoss ~ .,
                     training_set,
                     method="svmLinear",
                     trainControl=tc)
)
# random forest model

time_rf <- system.time(
  model_rf <- train(NetProfitLoss ~ .,
                    training_set,
                    method="rf",
                    trainControl=tc)
)

# xgboost model.

time_xgb <- system.time(
  model_xgb <- train(NetProfitLoss ~ .,
                     training_set,
                     method="xgbLinear",
                     trainControl=tc)
)

# ensemble of the three models.

time_ensemble <- system.time(
  model_list <- caretList(NetProfitLoss ~ ., 
                          data=training_set,
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
                     newdata=select(test_set, -NetProfitLoss))
#predictions <-predict(models_list, newdata=select(employee_dataset_test, -Attrition))

# confusion matrix evaluation results.

cm_metrics <- lapply(predictions,
                     confusionMatrix, 
                     reference=test_set$NetProfitLoss, 
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




