#with all data manipulation
# Outliar Imputation
#Scaling
#Dropping highly corelated columns

library(caret)
library(rpart.plot)
library(randomForest)
library(e1071)

orgData <- read.csv("D:/Imarticus_DSP/R Project/Attrition.csv")

head(orgData)
summary(orgData)
str(orgData)
anyNA(orgData)
summary(orgData$Attrition)


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
head(orgData[11]) #EnvironmentSatisfaction
head(orgData[17]) #JobSatisfaction
head(orgData[22]) #Over18
head(orgData[26]) #RelationshipSatisfaction
head(orgData[27]) #StandardHours

orgData <- orgData[-c(9,10,22,27)]

#### Outliar finding


boxplot(orgData["DistanceFromHome"])
boxplot(orgData["HourlyRate"])
boxplot(orgData["JobInvolvement"])
boxplot(orgData["JobLevel"])
boxplot(orgData["JobSatisfaction"])
boxplot(orgData["MonthlyIncome"]) #cluster of outliars
boxplot(orgData["MonthlyRate"])


boxplot(orgData["NumCompaniesWorked"]) # 1 outliar
summary(orgData$NumCompaniesWorked)
bench <- 4+1.5*IQR(orgData$NumCompaniesWorked) 
orgData$NumCompaniesWorked[orgData$NumCompaniesWorked>bench]<-bench
boxplot(orgData["NumCompaniesWorked"]) # 1 outliar

boxplot(orgData["PercentSalaryHike"])
boxplot(orgData["PerformanceRating"])  # 1 outliar - ignored as rating scale
boxplot(orgData["RelationshipSatisfaction"])

boxplot(orgData["StockOptionLevel"])  # 1 outliar
summary(orgData$StockOptionLevel)
bench1 <- 1+1.5*IQR(orgData$StockOptionLevel) 
orgData$StockOptionLevel[orgData$StockOptionLevel>bench1]<-bench1
summary(orgData$StockOptionLevel)
boxplot(orgData["StockOptionLevel"])


boxplot(orgData["TotalWorkingYears"])# cluster of outliars

boxplot(orgData["TrainingTimesLastYear"])# 3 outliars
summary(orgData$TrainingTimesLastYear)
bench2 <- 3+1.5*IQR(orgData$TrainingTimesLastYear) 
orgData$TrainingTimesLastYear[orgData$TrainingTimesLastYear>bench2]<-bench2
bench3 <- 2-1.5*IQR(orgData$TrainingTimesLastYear)
orgData$TrainingTimesLastYear[orgData$TrainingTimesLastYear<bench3]<-bench3
boxplot(orgData["TrainingTimesLastYear"])
summary(orgData["TrainingTimesLastYear"])


boxplot(orgData["WorkLifeBalance"])
boxplot(orgData["YearsAtCompany"])# cluster of outliars

boxplot(orgData["YearsInCurrentRole"]) # 4 outliars
summary(orgData$YearsInCurrentRole)
bench4 <- 7+1.5*IQR(orgData$YearsInCurrentRole) 
orgData$YearsInCurrentRole[orgData$YearsInCurrentRole>bench4]<-bench4
summary(orgData$YearsInCurrentRole)
boxplot(orgData["YearsInCurrentRole"])


boxplot(orgData["YearsSinceLastPromotion"])# 7-8 outliars
summary(orgData$YearsSinceLastPromotion)
bench5 <- 3+1.5*IQR(orgData$YearsSinceLastPromotion) 
orgData$YearsSinceLastPromotion[orgData$YearsSinceLastPromotion>bench5]<-bench5
summary(orgData$YearsSinceLastPromotion)
boxplot(orgData["YearsSinceLastPromotion"])

boxplot(orgData["YearsWithCurrManager"])  # 1 outliar
summary(orgData$YearsWithCurrManager)
bench6 <- 3+1.5*IQR(orgData$YearsWithCurrManager) 
orgData$YearsWithCurrManager[orgData$YearsWithCurrManager>bench6]<-bench6
summary(orgData$YearsWithCurrManager)
boxplot(orgData["YearsWithCurrManager"])

###################################

#Z-Score Standardization for Scaling Numeric Data

summary(orgData["Age"])
orgData["Age"] <- as.data.frame(scale(orgData["Age"]))
summary(orgData["Age"])

summary(orgData["DailyRate"])
orgData["DailyRate"] <- as.data.frame(scale(orgData["DailyRate"]))
summary(orgData["DailyRate"])

summary(orgData["DistanceFromHome"])
orgData["DistanceFromHome"] <- as.data.frame(scale(orgData["DistanceFromHome"]))
summary(orgData["DistanceFromHome"])

summary(orgData["Education"])
orgData["Education"] <- as.data.frame(scale(orgData["Education"]))
summary(orgData["Education"])

summary(orgData["EnvironmentSatisfaction"])
orgData["EnvironmentSatisfaction"] <- as.data.frame(scale(orgData["EnvironmentSatisfaction"]))
summary(orgData["EnvironmentSatisfaction"])


summary(orgData["HourlyRate"])
orgData["HourlyRate"] <- as.data.frame(scale(orgData["HourlyRate"]))
summary(orgData["HourlyRate"])

summary(orgData["JobInvolvement"])
orgData["JobInvolvement"] <- as.data.frame(scale(orgData["JobInvolvement"]))
summary(orgData["JobInvolvement"])

summary(orgData["JobLevel"])
orgData["JobLevel"] <- as.data.frame(scale(orgData["JobLevel"]))
summary(orgData["JobLevel"])

summary(orgData["JobSatisfaction"])
orgData["JobSatisfaction"] <- as.data.frame(scale(orgData["JobSatisfaction"]))
summary(orgData["JobSatisfaction"])


summary(orgData["MonthlyIncome"])
orgData["MonthlyIncome"] <- as.data.frame(scale(orgData["MonthlyIncome"]))
summary(orgData["MonthlyIncome"])

summary(orgData["MonthlyRate"])
orgData["MonthlyRate"] <- as.data.frame(scale(orgData["MonthlyRate"]))
summary(orgData["MonthlyRate"])

summary(orgData["NumCompaniesWorked"])
orgData["NumCompaniesWorked"] <- as.data.frame(scale(orgData["NumCompaniesWorked"]))
summary(orgData["NumCompaniesWorked"])

summary(orgData["PercentSalaryHike"])
orgData["PercentSalaryHike"] <- as.data.frame(scale(orgData["PercentSalaryHike"]))
summary(orgData["PercentSalaryHike"])

summary(orgData["PerformanceRating"])
orgData["PerformanceRating"] <- as.data.frame(scale(orgData["PerformanceRating"]))
summary(orgData["PerformanceRating"])

summary(orgData["RelationshipSatisfaction"])
orgData["RelationshipSatisfaction"] <- as.data.frame(scale(orgData["RelationshipSatisfaction"]))
summary(orgData["RelationshipSatisfaction"])

summary(orgData["StockOptionLevel"])
orgData["StockOptionLevel"] <- as.data.frame(scale(orgData["StockOptionLevel"]))
summary(orgData["StockOptionLevel"])

summary(orgData["TotalWorkingYears"])
orgData["TotalWorkingYears"] <- as.data.frame(scale(orgData["TotalWorkingYears"]))
summary(orgData["TotalWorkingYears"])

summary(orgData["TrainingTimesLastYear"])
orgData["TrainingTimesLastYear"] <- as.data.frame(scale(orgData["TrainingTimesLastYear"]))
summary(orgData["TrainingTimesLastYear"])

summary(orgData["WorkLifeBalance"])
orgData["WorkLifeBalance"] <- as.data.frame(scale(orgData["WorkLifeBalance"]))
summary(orgData["WorkLifeBalance"])

summary(orgData["YearsAtCompany"])
orgData["YearsAtCompany"] <- as.data.frame(scale(orgData["YearsAtCompany"]))
summary(orgData["YearsAtCompany"])

summary(orgData["YearsInCurrentRole"])
orgData["YearsInCurrentRole"] <- as.data.frame(scale(orgData["YearsInCurrentRole"]))
summary(orgData["YearsInCurrentRole"])

summary(orgData["YearsSinceLastPromotion"])
orgData["YearsSinceLastPromotion"] <- as.data.frame(scale(orgData["YearsSinceLastPromotion"]))
summary(orgData["YearsSinceLastPromotion"])

summary(orgData["YearsWithCurrManager"])
orgData["YearsWithCurrManager"] <- as.data.frame(scale(orgData["YearsWithCurrManager"]))
summary(orgData["YearsWithCurrManager"])

############# Splitting files

set.seed(300)
split <- sample (seq_len(nrow(orgData)), size = floor(0.70 * nrow(orgData)))
trainData <- orgData[split, ]
testData <- orgData[-split, ]

summary(as.factor(orgData$Attrition))
summary(as.factor(trainData$Attrition))
summary(as.factor(testData$Attrition))


write.csv(trainData, file = 'D:/Imarticus_DSP/R Project/Code file/trainData_after_mani.csv')
write.csv(testData, file = 'D:/Imarticus_DSP/R Project/Code file/testData_after_mani.csv')



################## model building without any data mmanupulation

lg_model1 <- glm(trainData$Attrition ~ ., family = binomial(link = 'logit'), data = trainData[-c(2)])
summary(lg_model1)

imp <- caret::varImp(lg_model1, scale=FALSE)

df <- data.frame(imp)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/lm1_importance_mani.csv')

log_predict <- predict(lg_model1, newdata = testData[-c(2)], type = "response")
log_predict <- ifelse(log_predict > 0.56,"Yes","No")

conf <-table(testData$Attrition, log_predict, dnn = c("Actual", "Predicted"))

confusionMatrix(data = as.factor(log_predict), reference = testData$Attrition, positive="Yes")


df = data.frame(as.factor(log_predict),testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/lm1_predict.csv')

#==== With all important varialbles based on p value


lg_model2 <- glm(trainData$Attrition ~ Age + BusinessTravel + DistanceFromHome + EnvironmentSatisfaction + Gender + JobInvolvement + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + RelationshipSatisfaction + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(link = 'logit'), data = trainData[-c(2)])

summary(lg_model2)

log_predict <- predict(lg_model2, newdata = testData[-c(2)], type = "response")
log_predict <- ifelse(log_predict > 0.53,"Yes","No")

conf <-table(testData$Attrition, log_predict, dnn = c("Actual", "Predicted"))

confusionMatrix(data = as.factor(log_predict), reference = testData$Attrition, positive="Yes")

df = data.frame(log_predict,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/lm2_predict.csv')


#==== With all important varialbles based on p values using chisq test


#lg_model3 <- glm(trainData$Attrition ~ Age + BusinessTravel + Department + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobLevel + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + TrainingTimesLastYear + WorkLifeBalance + YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(link = 'logit'), data = trainData[-c(2)])
lg_model4 <- glm(trainData$Attrition ~ Age + BusinessTravel + Department + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobLevel + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + RelationshipSatisfaction + TrainingTimesLastYear + TotalWorkingYears + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + Work_Happiness_Index, family = binomial(link = 'logit'), data = trainData[-c(2)])

#anova(lg_model3, test = 'Chisq')
anova(lg_model4, test = 'Chisq')

# log_predict <- predict(lg_model3, newdata = testData[-c(2)], type = "response")
# log_predict <- ifelse(log_predict > 0.41,"Yes","No")

log_predict <- predict(lg_model4, newdata = testData[-c(2)], type = "response")
log_predict <- ifelse(log_predict > 0.48,"Yes","No")

conf <-table(testData$Attrition, log_predict, dnn = c("Actual", "Predicted"))

confusionMatrix(data = as.factor(log_predict), reference = testData$Attrition, positive="Yes")

df = data.frame(log_predict,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/lm3_predict.csv')

df = data.frame(log_predict,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/lm4_predict.csv')

##################### DECISION TREE ######################



trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(3333)
dtree_fit <- train(Attrition ~ ., data = trainData,
                   method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 14)
dtree_fit
imp <- varImp(dtree_fit, scale=FALSE)
plot(imp)

df <- data.frame(imp$importance)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/dt1_importance.csv')

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

dt_pred <- predict(dtree_fit, newdata = testData)
confusionMatrix(data = dt_pred, reference = testData$Attrition, positive="Yes")

df = data.frame(dt_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/dt1_predict.csv')

### Important Variables


set.seed(3333)
dtree_fit2 <- train(Attrition ~ Age + BusinessTravel + DailyRate + Department + DistanceFromHome + EnvironmentSatisfaction + JobLevel + JobRole + MaritalStatus + MonthlyIncome + OverTime + StockOptionLevel + TotalWorkingYears + YearsAtCompany + JobInvolvement + PercentSalaryHike,
                    data = trainData,
                    method = "rpart",
                    parms = list(split = "information"),
                    trControl=trctrl,
                    tuneLength = 12)
dtree_fit2
imp <- varImp(dtree_fit2, scale=FALSE)

df <- data.frame(imp$importance)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/dt2_importance.csv')

prp(dtree_fit2$finalModel, box.palette = "Reds", tweak = 1.2)

dt_pred <- predict(dtree_fit2, newdata = testData)

confusionMatrix(data = dt_pred, reference = testData$Attrition, positive="Yes")

df = data.frame(dt_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/dt2_predict.csv')

#===================== gini index
#Training as criterion as GINI INDEX
set.seed(3333)
dtree_fit_gini <- train(Attrition ~ ., data = trainData, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 9)
dtree_fit_gini
imp <- varImp(dtree_fit_gini, scale=FALSE)

imp <- data.frame(imp$importance)
write.csv(imp, file = 'D:/Imarticus_DSP/R Project/Code file/gdt1_importance.csv')

prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)

dt_gini_pred <- predict(dtree_fit_gini, newdata = testData)
confusionMatrix(data = dt_gini_pred, reference = testData$Attrition, positive="Yes")

df = data.frame(dt_gini_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/dt_gini_predict1.csv')


### with imp variables

dtree_fit_gini2 <- train(Attrition ~ MonthlyIncome + Age + TotalWorkingYears + YearsAtCompany + OverTime + JobLevel + StockOptionLevel + MaritalStatus + DistanceFromHome + BusinessTravel + EnvironmentSatisfaction + JobRole + PercentSalaryHike + Department + DailyRate + YearsSinceLastPromotion + TrainingTimesLastYear + WorkLifeBalance + YearsInCurrentRole, data = trainData, method = "rpart",
                         parms = list(split = "gini"),
                         trControl=trctrl,
                         tuneLength = 9)
dtree_fit_gini2

prp(dtree_fit_gini2$finalModel, box.palette = "Blues", tweak = 1.2)

dt_gini_pred <- predict(dtree_fit_gini2, newdata = testData)
confusionMatrix(data = dt_gini_pred, reference = testData$Attrition, positive="Yes")

df = data.frame(dt_gini_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/dt_gini_predict2.csv')

########### RANDOM FOREST

set.seed(500)

# will take longer as we are creating 2000 decision trees (uses bagging internally)
#bagging example - > sample(1:10, replace = TRUE)
# run above line everytime and it gneartes 10 numbers but different values each time (between 1 to 10)
rfit <- randomForest(trainData$Attrition ~ .,
                     data=trainData, 
                     importance=TRUE, 
                     ntree=500)

varImp(rfit)
varUsed(rfit, by.tree=FALSE, count=TRUE)
imp <- importance(rfit)
imp <- as.data.frame(imp)
df_acc <- data.frame(imp[3])
df_gini <- data.frame(imp[4])

write.csv(df_acc, file = 'D:/Imarticus_DSP/R Project/Code file/rf_important_acc_mani.csv')
write.csv(df_gini, file = 'D:/Imarticus_DSP/R Project/Code file/rf_important_gini_mani.csv')

varImpPlot(rfit)

rf_pred <- predict(rfit, testData)

table(testData$Attrition,rf_pred)
confusionMatrix(data = rf_pred, reference = testData$Attrition, positive="Yes")

df = data.frame(rf_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/rf_predict1.csv')

#####################################
set.seed(500)

# will take longer as we are creating 2000 decision trees (uses bagging internally)
#bagging example - > sample(1:10, replace = TRUE)
# run above line everytime and it gneartes 10 numbers but different values each time (between 1 to 10)
rfit2 <- randomForest(trainData$Attrition ~ EducationField + EnvironmentSatisfaction + Gender + YearsInCurrentRole + OverTime + WorkLifeBalance + JobRole + JobInvolvement + MonthlyIncome + PercentSalaryHike + Work_Happiness_Index + TotalWorkingYears + JobLevel + Age + YearsAtCompany + BusinessTravel + RelationshipSatisfaction + YearsWithCurrManager,
                      data=trainData, 
                      importance=TRUE, 
                      ntree=85)



varImpPlot(rfit2)

rf_pred <- predict(rfit2, testData)

table(testData$Attrition,rf_pred)
confusionMatrix(data = rf_pred, reference = testData$Attrition, positive="Yes")

df = data.frame(rf_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/rf_predict2.csv')

#####################################
set.seed(500)

# will take longer as we are creating 2000 decision trees (uses bagging internally)
#bagging example - > sample(1:10, replace = TRUE)
# run above line everytime and it gneartes 10 numbers but different values each time (between 1 to 10)
rfit3 <- randomForest(trainData$Attrition ~ MonthlyIncome + Age + OverTime + DistanceFromHome + DailyRate + Work_Happiness_Index + TotalWorkingYears + MonthlyRate + HourlyRate + JobRole + YearsAtCompany + StockOptionLevel + EnvironmentSatisfaction + NumCompaniesWorked,
                      data=trainData, 
                      importance=TRUE, 
                      ntree=55)

# #need to run this using 19 variables
# rfit4 <- randomForest(trainData$Attrition ~ MonthlyIncome + Age + OverTime + DistanceFromHome + DailyRate + Work_Happiness_Index + TotalWorkingYears + MonthlyRate + HourlyRate + JobRole + YearsAtCompany + StockOptionLevel + EnvironmentSatisfaction + NumCompaniesWorked + PercentSalaryHike + YearsWithCurrManager + EducationField + WorkLifeBalance + TrainingTimesLastYear,
#                        data=trainData, 
#                        importance=TRUE, 
#                        ntree=2900)

#varImpPlot(rfit3)

rf_pred <- predict(rfit3, testData)
# rf_pred <- predict(rfit4, testData)

table(testData$Attrition,rf_pred)
confusionMatrix(data = rf_pred, reference = testData$Attrition, positive="Yes")

df = data.frame(rf_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/rf_predict3.csv')


##########

set.seed(500)

svm1 <- svm(Attrition ~ ., data=trainData)
svm_pred <- predict(svm1,testData)
print(table(svm_pred,testData$Attrition))
confusionMatrix(data = as.factor(svm_pred), reference = testData$Attrition, positive="Yes")

df = data.frame(svm_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/svm_predict1.csv')

svm2 <- svm(Attrition ~ ., data=trainData, kernel="radial", cost=7, gamma=0.015)
svm_pred <- predict(svm2,testData)
print(table(svm_pred,testData$Attrition))
confusionMatrix(data = as.factor(svm_pred), reference = testData$Attrition, positive="Yes")


df = data.frame(svm_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/svm_predict2.csv')

library(rminer) # used for using fit model function and deriving variable importance by svm model

imp <- fit(Attrition~., data=trainData, model="svm", kpar=list(sigma=0.015), C=9)
svm_imp <- Importance(imp, data=trainData)
svm_imp$imp
df <- data.frame(colnames(trainData),svm_imp$imp)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/svm2_importance.csv')

svm3 <- svm(Attrition ~ YearsAtCompany + OverTime + NumCompaniesWorked + DistanceFromHome + JobInvolvement + WorkLifeBalance + YearsSinceLastPromotion + MonthlyIncome + JobSatisfaction + EnvironmentSatisfaction + TotalWorkingYears + RelationshipSatisfaction + Work_Happiness_Index + BusinessTravel + TrainingTimesLastYear + JobLevel + Department + EducationField + Age + Gender + StockOptionLevel, data=trainData, kernel="radial", cost=10, gamma=0.005)
svm_pred <- predict(svm3,testData)
print(table(svm_pred,testData$Attrition))
confusionMatrix(data = as.factor(svm_pred), reference = testData$Attrition, positive="Yes")

df = data.frame(svm_pred,testData$Attrition)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/svm_predict3.csv')

