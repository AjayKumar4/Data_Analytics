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


#setting Workspace path
setwd("~/Workspace/Data_Analytics")


# Importing the dataset
economic_dataset <- read.csv('economic.csv', header=TRUE, sep=",", stringsAsFactors=FALSE, fileEncoding="latin1")
fish_dataset <- read.csv('fish2.csv', header=TRUE, sep=",", stringsAsFactors=FALSE, fileEncoding="latin1")
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