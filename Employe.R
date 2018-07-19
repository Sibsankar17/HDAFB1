library(readxl)
 data_Train <-  read_excel("D:/Employee Attrition Dataset_Problem.xlsx", sheet = "Train")
 sub_Train <- subset(data_Train, select = c(39:length(data_Train)-1))
 #library(mlbench)
 data(data_Train, package="mlbench")

options(scipen=999)  # prevents printing scientific notations.
set.seed(100)
trainDataIndex <- createDataPartition(sub_Train$Attrition1, p=0.7, list = F)
train_Data <- sub_Train[trainDataIndex, ]
test_Data <-  sub_Train[-trainDataIndex, ]

#for(i in 2:29) { sub_Train[, i] <- as.numeric(as.character(sub_Train[, i])) };
#train_Data$Attrition <- ifelse(train_Data$Attrition == "Yes", 1, 0)
sub_Train$Attrition1 <- factor(sub_Train$Attrition1, levels = c(0, 1))
# Down Sample
View(sub_Train)
require(caret)
'%ni%' <- Negate('%in%')
set.seed(100)
down_train <- upSample(x = sub_Train[, colnames(sub_Train) %ni% "Attrition1"],
                         y = sub_Train$Attrition1)
View(down_train)

table(down_train$Class)
#up_train <- upSample(x = train_Data[, colnames(train_Data) %ni% "Class"], y = trainData$Class)

#trainning <- subset(train_Data, select = c(39:length(train_Data)-1))

#test_Data <-  read_excel("D:/Employee Attrition Dataset_Problem.xlsx", sheet = "Test")
##testing <- subset(test_Data, select = c(37:length(test_Data)-1))
library(caTools)

model <- glm(Class~.-HourlyRate1-MonthlyRate1-Education1-PercentSalaryHike1,data=down_train, family = "binomial")
summary(model)
#resTrain <- predict(model, trainning, type ="response")
 pred <- predict(model, test_Data, type ="response")

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test_Data$Attrition1

# Accuracy
mean(y_pred == y_act)  # 94%
#View(resTest <- ifelse(resTest >.5, 1,0))
#test_Data$AttritionResult <- ifelse(resTest >.5, 1,0)

#table(AcualValue=trainning$Attrition1, PredictedValue=resTrain>0.5)
#write.xlsx(test_Data, "D:/Employee Attrition Dataset_Solv1.xlsx", sheet="Result")
