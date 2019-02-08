#### THIS FILE CONTAINS CODE FOR
#### ALL MODELS CREATED USING ALL VARIABLES OF THE DATA FILE


library(caret)

#reading the orginal data file
orgData <- read.csv("D:/Imarticus_DSP/R Project/Attrition.csv")

head(orgData)
summary(orgData)
str(orgData)
anyNA(orgData)
summary(orgData$Attrition)

##### created new variable "Work_Happiness_Index" from exisiting three variables
##### work Happiness index calculation

summary(as.factor(orgData$EnvironmentSatisfaction))
summary(as.factor(orgData$JobSatisfaction))
summary(as.factor(orgData$RelationshipSatisfaction))

orgData["Work_Happiness_Index"] <- ifelse((orgData$JobSatisfaction >= 3 & orgData$EnvironmentSatisfaction >=3 & orgData$RelationshipSatisfaction>=3),"Very Happy",
                                          ifelse((orgData$JobSatisfaction >= 3 & (orgData$EnvironmentSatisfaction >=3 | orgData$RelationshipSatisfaction>=3)),"Happy",
                                                 ifelse((orgData$JobSatisfaction <=2 & (orgData$EnvironmentSatisfaction >=3 | orgData$RelationshipSatisfaction >=3)),"Sad",
                                                        ifelse((orgData$JobSatisfaction <=2 & orgData$EnvironmentSatisfaction <=2 & orgData$RelationshipSatisfaction<=2),"Very Sad","Somewhat Happy"))))

summary(as.factor(orgData$Work_Happiness_Index))
orgData$Work_Happiness_Index <- as.factor(orgData$Work_Happiness_Index)


############## Domain Knowledge Variable Removal  ###############

# Removing columns with only 1 Level = "EmployeeCount","Over18", "StandardHours"
# Removing column "EmployeeNumber" as it has unqiue values

head(orgData[9]) #EmployeeCount
head(orgData[10]) #EmployeeNumber
head(orgData[22]) #Over18
head(orgData[27]) #StandardHours

orgData <- orgData[-c(9,10,22,27)]

############# Splitting files at 70:30 ratio

set.seed(300)
split <- sample (seq_len(nrow(orgData)), size = floor(0.70 * nrow(orgData)))
trainData <- orgData[split, ]
testData <- orgData[-split, ]

############# Checking Yes/No counts for Target Variable "Attrition"

summary(as.factor(orgData$Attrition))
summary(as.factor(trainData$Attrition))
summary(as.factor(testData$Attrition))


write.csv(trainData, file = 'D:/Imarticus_DSP/R Project/Code file/trainData.csv')
write.csv(testData, file = 'D:/Imarticus_DSP/R Project/Code file/testData.csv')



################## model building without any data mmanupulation

################## Logistic Regression Model

lg_model1 <- glm(trainData$Attrition ~ ., family = binomial(link = 'logit'), data = trainData[-c(2)])
summary(lg_model1)

log_predict <- predict(lg_model1, newdata = testData[-c(2)], type = "response")
log_predict <- ifelse(log_predict > 0.51,"Yes","No")

conf <-table(testData$Attrition, log_predict, dnn = c("Actual", "Predicted"))

confusionMatrix(data = as.factor(log_predict), reference = testData$Attrition, positive="Yes")


df = data.frame(as.factor(log_predict),testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/lm1_predict.csv')


##################### DECISION TREE  = Information Gain method ######################


library(rpart.plot)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(3333)
dtree_fit <- train(Attrition ~ ., data = trainData,
                   method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 14)

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

dt_pred <- predict(dtree_fit, newdata = testData)
confusionMatrix(data = dt_pred, reference = testData$Attrition, positive="Yes")

df = data.frame(dt_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/dt1_predict.csv')


##################### DECISION TREE  = Gini Index  method ######################
set.seed(3333)
dtree_fit_gini <- train(Attrition ~ ., data = trainData, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 9)

prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)

dt_gini_pred <- predict(dtree_fit_gini, newdata = testData)
confusionMatrix(data = dt_gini_pred, reference = testData$Attrition, positive="Yes")

df = data.frame(dt_gini_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/dt_gini_predict1.csv')



##################### RANDOM FOREST ######################

library(randomForest)

set.seed(500)

# will take longer as we are creating 2000 decision trees (uses bagging internally)
#bagging example - > sample(1:10, replace = TRUE)
# run above line everytime and it gneartes 10 numbers but different values each time (between 1 to 10)
rfit <- randomForest(trainData$Attrition ~ .,
                    data=trainData, 
                    importance=TRUE, 
                    ntree=5)

varImpPlot(rfit)

rf_pred <- predict(rfit, testData)

table(testData$Attrition,rf_pred)
confusionMatrix(data = rf_pred, reference = testData$Attrition, positive="Yes")

df = data.frame(rf_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/rf_predict1.csv')

##################### SVM models ######################

library("e1071")

##################### Basic SVM model ######################

set.seed(500)

svm1 <- svm(Attrition ~ ., data=trainData)
svm_pred <- predict(svm1,testData)
print(table(svm_pred,testData$Attrition))
confusionMatrix(data = as.factor(svm_pred), reference = testData$Attrition, positive="Yes")

df = data.frame(svm_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/svm_predict1.csv')

#####################  Tunned SVM model using cost and gamma ######################

svm2 <- svm(Attrition ~ ., data=trainData, kernel="radial", cost=9, gamma=0.015)
svm_pred <- predict(svm2,testData)
print(table(svm_pred,testData$Attrition))
confusionMatrix(data = as.factor(svm_pred), reference = testData$Attrition, positive="Yes")


df = data.frame(svm_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/svm_predict2.csv')

