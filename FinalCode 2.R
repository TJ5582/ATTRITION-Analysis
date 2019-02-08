library(caret)

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

############# Splitting files

set.seed(300)
split <- sample (seq_len(nrow(orgData)), size = floor(0.70 * nrow(orgData)))
trainData <- orgData[split, ]
testData <- orgData[-split, ]

summary(as.factor(orgData$Attrition))
summary(as.factor(trainData$Attrition))
summary(as.factor(testData$Attrition))


write.csv(trainData, file = 'D:/Imarticus_DSP/R Project/Code file/trainData_mani.csv')
write.csv(testData, file = 'D:/Imarticus_DSP/R Project/Code file/testData_mani.csv')



################## model building without any data mmanupulation

lg_model1 <- glm(trainData$Attrition ~ ., family = binomial(link = 'logit'), data = trainData[-c(2)])
summary(lg_model1)

imp <- caret::varImp(lg_model1, scale=FALSE)

df <- data.frame(imp)
write.csv(df, file = 'D:/Imarticus_DSP/R Project/Code file/lm1_importance_mani.csv')

thresh_range = seq(from = 0.25, to = 0.75, by=0.01)

lg_df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(lg_df) <- c("Threshold","Accuracy","Kappa","Sensitivity","Specificity","Pos_Pred_Value","Neg_Pred_Value","Total_Err","T1_Err","T2_Err")

for (thresh in thresh_range){ 
  log_predict <- predict(lg_model1, newdata = testData[-c(2)], type = "response")
  log_predict <- ifelse(log_predict > thresh,"Yes","No")
  conf <-table(testData$Attrition, log_predict, dnn = c("Actual", "Predicted"))
  print(conf)
  print(paste("====================Threshold =",thresh,"================================"))
  cm <- confusionMatrix(data = as.factor(log_predict), reference = testData$Attrition, positive="Yes")
  lg_df <- rbind(lg_df, data.frame(Threshold=thresh,Accuracy=cm$overall[1],Kappa=cm$overall[2],Sensitivity=cm$byClass[1],Specificity=cm$byClass[2],Pos_Pred_Value=cm$byClass[3],Neg_Pred_Value=cm$byClass[4],Total_Err=(cm$table[2]+cm$table[3]),T1_Err=cm$table[2],T2_Err=cm$table[3]))
}

write.csv(lg_df, file = 'D:/Imarticus_DSP/R Project/Code file/cm_log1_mani2.csv')


#==== With all important varialbles based on p value


lg_model2 <- glm(trainData$Attrition ~ Age + BusinessTravel 
                 + DistanceFromHome + EnvironmentSatisfaction 
                 + Gender + JobInvolvement
                 + JobSatisfaction + MaritalStatus
                 + NumCompaniesWorked + OverTime
                 + RelationshipSatisfaction + TotalWorkingYears 
                 + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole 
                 + YearsSinceLastPromotion + YearsWithCurrManager
                 , family = binomial(link = 'logit'), data = trainData[-c(2)])

summary(lg_model2)

thresh_range = seq(from = 0.25, to = 0.75, by=0.01)

lg_df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(lg_df) <- c("Threshold","Accuracy","Kappa","Sensitivity","Specificity","Pos_Pred_Value","Neg_Pred_Value","Total_Err","T1_Err","T2_Err")

for (thresh in thresh_range){ 
  log_predict <- predict(lg_model2, newdata = testData[-c(2)], type = "response")
  log_predict <- ifelse(log_predict > thresh,"Yes","No")
  conf <-table(testData$Attrition, log_predict, dnn = c("Actual", "Predicted"))
  print(conf)
  print(paste("====================Threshold =",thresh,"================================"))
  cm <- confusionMatrix(data = as.factor(log_predict), reference = testData$Attrition, positive="Yes")
  lg_df <- rbind(lg_df, data.frame(Threshold=thresh,Accuracy=cm$overall[1],Kappa=cm$overall[2],Sensitivity=cm$byClass[1],Specificity=cm$byClass[2],Pos_Pred_Value=cm$byClass[3],Neg_Pred_Value=cm$byClass[4],Total_Err=(cm$table[2]+cm$table[3]),T1_Err=cm$table[2],T2_Err=cm$table[3]))
}

write.csv(lg_df, file = 'D:/Imarticus_DSP/R Project/Code file/cm_log2_mani.csv')


#==== With all important varialbles based on p values using chisq test

lg_model3 <- glm(trainData$Attrition ~ Age + BusinessTravel + Department + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobLevel + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + TrainingTimesLastYear + WorkLifeBalance + YearsSinceLastPromotion + YearsWithCurrManager, family = binomial(link = 'logit'), data = trainData[-c(2)])
lg_model4 <- glm(trainData$Attrition ~ Age + BusinessTravel + Department + DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + JobLevel + JobSatisfaction + MaritalStatus + NumCompaniesWorked + OverTime + RelationshipSatisfaction + TrainingTimesLastYear + TotalWorkingYears + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager + Work_Happiness_Index, family = binomial(link = 'logit'), data = trainData[-c(2)])

anova(lg_model3, test = 'Chisq')
anova(lg_model4, test = 'Chisq')

thresh_range = seq(from = 0.25, to = 0.75, by=0.01)

lg_df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(lg_df) <- c("Threshold","Accuracy","Kappa","Sensitivity","Specificity","Pos_Pred_Value","Neg_Pred_Value","Total_Err","T1_Err","T2_Err")

for (thresh in thresh_range){ 
  log_predict <- predict(lg_model3, newdata = testData[-c(2)], type = "response")
  log_predict <- ifelse(log_predict > thresh,"Yes","No")
  conf <-table(testData$Attrition, log_predict, dnn = c("Actual", "Predicted"))
  print(conf)
  print(paste("====================Threshold =",thresh,"================================"))
  cm <- confusionMatrix(data = as.factor(log_predict), reference = testData$Attrition, positive="Yes")
  lg_df <- rbind(lg_df, data.frame(Threshold=thresh,Accuracy=cm$overall[1],Kappa=cm$overall[2],Sensitivity=cm$byClass[1],Specificity=cm$byClass[2],Pos_Pred_Value=cm$byClass[3],Neg_Pred_Value=cm$byClass[4],Total_Err=(cm$table[2]+cm$table[3]),T1_Err=cm$table[2],T2_Err=cm$table[3]))
}

write.csv(lg_df, file = 'D:/Imarticus_DSP/R Project/Code file/cm_log3_mani.csv')

thresh_range = seq(from = 0.25, to = 0.75, by=0.01)

lg_df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(lg_df) <- c("Threshold","Accuracy","Kappa","Sensitivity","Specificity","Pos_Pred_Value","Neg_Pred_Value","Total_Err","T1_Err","T2_Err")

for (thresh in thresh_range){ 
  log_predict <- predict(lg_model4, newdata = testData[-c(2)], type = "response")
  log_predict <- ifelse(log_predict > thresh,"Yes","No")
  conf <-table(testData$Attrition, log_predict, dnn = c("Actual", "Predicted"))
  print(conf)
  print(paste("====================Threshold =",thresh,"================================"))
  cm <- confusionMatrix(data = as.factor(log_predict), reference = testData$Attrition, positive="Yes")
  lg_df <- rbind(lg_df, data.frame(Threshold=thresh,Accuracy=cm$overall[1],Kappa=cm$overall[2],Sensitivity=cm$byClass[1],Specificity=cm$byClass[2],Pos_Pred_Value=cm$byClass[3],Neg_Pred_Value=cm$byClass[4],Total_Err=(cm$table[2]+cm$table[3]),T1_Err=cm$table[2],T2_Err=cm$table[3]))
}

write.csv(lg_df, file = 'D:/Imarticus_DSP/R Project/Code file/cm_log4_mani.csv')

##################### DECISION TREE ######################


library(rpart.plot)
library(caret)


dt_tunelng = seq(from = 1, to = 35, by=1)

dt_df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(dt_df) <- c("Threshold","Accuracy","Kappa","Sensitivity","Specificity","Pos_Pred_Value","Neg_Pred_Value","Total_Err","T1_Err","T2_Err")
dt_df

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

for (len in dt_tunelng) {print(paste("====================Tune Length =",len,"================================")); set.seed(3333); dtree_model1 <- train(Attrition ~ ., data = trainData, method = "rpart", parms = list(split = "information"), trControl=trctrl, tuneLength = len); print(dtree_model1); dt_pred <- predict(dtree_model1, newdata = testData); cm <- confusionMatrix(data = as.factor(dt_pred), reference = testData$Attrition, positive="Yes"); print(cm); dt_df <- rbind(dt_df, data.frame(Tune_Length=len,Accuracy=cm$overall[1],Kappa=cm$overall[2],Sensitivity=cm$byClass[1],Specificity=cm$byClass[2],Pos_Pred_Value=cm$byClass[3],Neg_Pred_Value=cm$byClass[4],Total_Err=(cm$table[2]+cm$table[3]),T1_Err=cm$table[2],T2_Err=cm$table[3]))}

prp(dtree_model1$finalModel, box.palette = "Reds", tweak = 1.2)

write.csv(dt_df, file = 'D:/Imarticus_DSP/R Project/Code file/cm_dt1_mani2.csv')


### Important Variables

dt_tunelng = seq(from = 1, to = 20, by=1)

dt_df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(dt_df) <- c("Tune_Length","Accuracy","Kappa","Sensitivity","Specificity","Pos_Pred_Value","Neg_Pred_Value","Total_Err","T1_Err","T2_Err")
dt_df

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

for (len in dt_tunelng) {print(paste("====================Tune Length =",len,"================================")); set.seed(3333); dtree_model2 <- train(Attrition ~ Age + BusinessTravel + DailyRate + Department + DistanceFromHome + EnvironmentSatisfaction + JobLevel + JobRole + MaritalStatus + MonthlyIncome + OverTime + StockOptionLevel + TotalWorkingYears + YearsAtCompany + JobInvolvement + PercentSalaryHike, data = trainData, method = "rpart", parms = list(split = "information"), trControl=trctrl, tuneLength = len); print(dtree_model2); dt_pred <- predict(dtree_model2, newdata = testData); cm <- confusionMatrix(data = as.factor(dt_pred), reference = testData$Attrition, positive="Yes"); print(cm); dt_df <- rbind(dt_df, data.frame(Tune_Length=len,Accuracy=cm$overall[1],Kappa=cm$overall[2],Sensitivity=cm$byClass[1],Specificity=cm$byClass[2],Pos_Pred_Value=cm$byClass[3],Neg_Pred_Value=cm$byClass[4],Total_Err=(cm$table[2]+cm$table[3]),T1_Err=cm$table[2],T2_Err=cm$table[3]))}

prp(dtree_model2$finalModel, box.palette = "Reds", tweak = 1.2)

write.csv(dt_df, file = 'D:/Imarticus_DSP/R Project/Code file/cm_dt2_mani.csv')


#===================== gini index
#Training as criterion as GINI INDEX
dt_tunelng = seq(from = 1, to = 35, by=1)

dt_df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(dt_df) <- c("Tune_Length","Accuracy","Kappa","Sensitivity","Specificity","Pos_Pred_Value","Neg_Pred_Value","Total_Err","T1_Err","T2_Err")
dt_df

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

for (len in dt_tunelng) {print(paste("====================Tune Length =",len,"================================")); set.seed(3333); dtree_fit_gini <- train(Attrition ~ ., data = trainData, method = "rpart", parms = list(split = "gini"), trControl=trctrl, tuneLength = len); print(dtree_fit_gini); dt_pred <- predict(dtree_fit_gini, newdata = testData); cm <- confusionMatrix(data = as.factor(dt_pred), reference = testData$Attrition, positive="Yes"); print(cm); dt_df <- rbind(dt_df, data.frame(Tune_Length=len,Accuracy=cm$overall[1],Kappa=cm$overall[2],Sensitivity=cm$byClass[1],Specificity=cm$byClass[2],Pos_Pred_Value=cm$byClass[3],Neg_Pred_Value=cm$byClass[4],Total_Err=(cm$table[2]+cm$table[3]),T1_Err=cm$table[2],T2_Err=cm$table[3]))}

write.csv(dt_df, file = 'D:/Imarticus_DSP/R Project/Code file/cm_gini_dt1_mani2.csv')

prp(dtree_fit_gini$finalModel, box.palette = "Reds", tweak = 1.2)


### with imp variables

dt_tunelng = seq(from = 1, to = 20, by=1)

dt_df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(dt_df) <- c("Tune_Length","Accuracy","Kappa","Sensitivity","Specificity","Pos_Pred_Value","Neg_Pred_Value","Total_Err","T1_Err","T2_Err")
dt_df

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

for (len in dt_tunelng) {print(paste("====================Tune Length =",len,"================================")); set.seed(3333); dtree_fit_gini2 <- train(Attrition ~ MonthlyIncome + Age + TotalWorkingYears + YearsAtCompany + OverTime + JobLevel + StockOptionLevel + MaritalStatus + DistanceFromHome + BusinessTravel + EnvironmentSatisfaction + JobRole + PercentSalaryHike + Department + DailyRate + YearsSinceLastPromotion + TrainingTimesLastYear + WorkLifeBalance + YearsInCurrentRole, data = trainData, method = "rpart", parms = list(split = "gini"), trControl=trctrl, tuneLength = len); print(dtree_fit_gini2); dt_pred <- predict(dtree_fit_gini2, newdata = testData); cm <- confusionMatrix(data = as.factor(dt_pred), reference = testData$Attrition, positive="Yes"); print(cm); dt_df <- rbind(dt_df, data.frame(Tune_Length=len,Accuracy=cm$overall[1],Kappa=cm$overall[2],Sensitivity=cm$byClass[1],Specificity=cm$byClass[2],Pos_Pred_Value=cm$byClass[3],Neg_Pred_Value=cm$byClass[4],Total_Err=(cm$table[2]+cm$table[3]),T1_Err=cm$table[2],T2_Err=cm$table[3]))}

write.csv(dt_df, file = 'D:/Imarticus_DSP/R Project/Code file/cm_gini_dt2_mani.csv')

prp(dtree_fit_gini2$finalModel, box.palette = "Reds", tweak = 1.2)



########### RANDOM FOREST

# will take longer as we are creating 2000 decision trees (uses bagging internally)
#bagging example - > sample(1:10, replace = TRUE)
# run above line everytime and it gneartes 10 numbers but different values each time (between 1 to 10)
library(randomForest)

ntree_val = c(seq(from = 5, to = 100, by=5),seq(from = 100, to = 3000, by=100))

#seq(from = 5, to = 100, by=5), seq(from = 100, to = 3000, by=100)

rf_df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(rf_df) <-  c("Num_Trees","Accuracy","Kappa","Sensitivity","Specificity","Pos_Pred_Value","Neg_Pred_Value","Total_Err","T1_Err","T2_Err")
rf_df

for (num_tree in ntree_val){ print(paste("================== Tree Value =",num_tree,"====================")); set.seed(500); rf_model1 <- randomForest(trainData$Attrition ~ MonthlyIncome + Age + OverTime + DistanceFromHome + DailyRate + Work_Happiness_Index + TotalWorkingYears + MonthlyRate + HourlyRate + JobRole + YearsAtCompany + StockOptionLevel + EnvironmentSatisfaction + NumCompaniesWorked, data=trainData, importance=TRUE, ntree=num_tree); rf_pred <- predict(rf_model1, testData); rfcm <- confusionMatrix(data = as.factor(rf_pred), reference = testData$Attrition, positive="Yes"); rf_df <- rbind(rf_df, data.frame(Num_Trees=num_tree, Accuracy=rfcm$overall[1], Kappa=rfcm$overall[2], Sensitivity=rfcm$byClass[1], Specificity=rfcm$byClass[2], Pos_Pred_Value=rfcm$byClass[3], Neg_Pred_Value=rfcm$byClass[4], Total_Err=(rfcm$table[2]+rfcm$table[3]), T1_Err=rfcm$table[2], T2_Err=rfcm$table[3])) }

write.csv(rf_df, file = 'D:/Imarticus_DSP/R Project/Code file/cm_rf4_mani.csv')



##########
library("e1071")

set.seed(500)

svm_mod_acc <- data.frame(matrix(ncol = 11, nrow = 0))
cols <- c("Cost","Gamma","Accuracy","Kappa","Sensitivity","Specificity","Pos_Pred_Value","Neg_Pred_Value","Total_Err","T1_Err","T2_Err")
colnames(svm_mod_acc) <- cols

svm_cost <- c(seq(1,9,1),seq(10,100,10))
svm_gm <- c(seq(0.000,0.1,0.005),seq(0.1,1.0,0.1))
#svm_gm <- c(seq(1,9,1),seq(10,100,10))

#,seq(1.0,9.0,1.0),seq(10.0,100.0,10.0)

#higher the cost better the classification
#lower gamma means tighter boundary. Close by points for classification
for (cst in svm_cost){
  for (gm in svm_gm){
    print(paste("========== Cost =",cst,"    Gamma=",gm," ============="))
    svm1 <- svm(Attrition ~ YearsAtCompany + OverTime + NumCompaniesWorked + DistanceFromHome + JobInvolvement + WorkLifeBalance + YearsSinceLastPromotion + MonthlyIncome + JobSatisfaction + EnvironmentSatisfaction + TotalWorkingYears + RelationshipSatisfaction + Work_Happiness_Index + BusinessTravel + TrainingTimesLastYear + JobLevel + Department + EducationField + Age + Gender + StockOptionLevel, data=trainData, kernel="radial", cost=cst, gamma=gm)
    pred <- predict(svm1,testData)
    svm_cm <- confusionMatrix(data = as.factor(pred), reference = testData$Attrition, positive="Yes")
    svm_mod_acc <- rbind(svm_mod_acc, data.frame(Cost=cst,Gamma=gm,Accuracy=svm_cm$overall[1],Kappa=svm_cm$overall[2],Sensitivity=svm_cm$byClass[1],Specificity=svm_cm$byClass[2],Pos_Pred_Value=svm_cm$byClass[3],Neg_Pred_Value=svm_cm$byClass[4],Total_Err=(svm_cm$table[2]+svm_cm$table[3]),T1_Err=svm_cm$table[2],T2_Err=svm_cm$table[3]))
  }
}

write.csv(svm_mod_acc, file = 'D:/Imarticus_DSP/R Project/Code file/cm_svm1_mani.csv')


